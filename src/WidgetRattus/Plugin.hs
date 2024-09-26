{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}


-- | The plugin to make it all work.

module WidgetRattus.Plugin (plugin, WidgetRattus(..)) where
import WidgetRattus.Plugin.StableSolver
import WidgetRattus.Plugin.ScopeCheck
import WidgetRattus.Plugin.Strictify
import WidgetRattus.Plugin.SingleTick
import WidgetRattus.Plugin.CheckClockCompatibility
import WidgetRattus.Plugin.Utils
import WidgetRattus.Plugin.Annotation
import WidgetRattus.Plugin.Transform

import Prelude hiding ((<>))

import Control.Monad
import Data.Maybe
import Data.List
import Data.Data hiding (tyConName)
import qualified Data.Set as Set
import Data.Set (Set)

import qualified GHC.LanguageExtensions as LangExt

import GHC.Plugins
import GHC.Tc.Types

-- | Use this to enable Asynchronous Rattus' plugin, either by supplying the option
-- @-fplugin=WidgetRattus.Plugin@ directly to GHC, or by including the
-- following pragma in each source file:
-- 
-- > {-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install,
  pluginRecompile = purePlugin,
  typeCheckResultAction = typechecked,
  tcPlugin = tcStable,
  driverPlugin = updateEnv
  }


data Options = Options {debugMode :: Bool}


-- | Enable the @Strict@ language extension.
updateEnv :: [CommandLineOption] -> HscEnv -> IO HscEnv
updateEnv _ env = return env {hsc_dflags = update (hsc_dflags env) } 
  where update fls = xopt_set fls LangExt.Strict

typechecked :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
typechecked _ _ env = checkAll env >> return env

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opts todo = case find findSamePass todo of       -- check that we don't run the transformation twice
                      Nothing -> return (strPass : todo) -- (e.g. if the "-fplugin" option is used twice)
                      _ -> return todo
    where name = "Async Rattus strictify"
          strPass = CoreDoPluginPass name (strictifyProgram Options{debugMode = dmode})
          dmode = "debug" `elem` opts
          findSamePass (CoreDoPluginPass s _) = s == name
          findSamePass _ = False
          

-- | Apply the following operations to all Asynchronous Rattus definitions in the
-- program:
--
-- * Transform into single tick form (see SingleTick module)
-- * Check whether lazy data types are used (see Strictify module)
-- * Transform into call-by-value form (see Strictify module)

strictifyProgram :: Options -> ModGuts -> CoreM ModGuts
strictifyProgram opts guts = do
  newBinds <- mapM (strictify opts guts) (mg_binds guts)
  return guts { mg_binds = newBinds }

strictify :: Options -> ModGuts -> CoreBind -> CoreM CoreBind
strictify opts guts b@(Rec bs) = do
  let debug = debugMode opts
  tr <- liftM or (mapM (shouldProcessCore guts . fst) bs)
  if tr then do
    let vs = map fst bs
    es' <- mapM (\ (v,e) -> do
      processCore <- shouldProcessCore guts v
      if not processCore
      then do
        when debug $ putMsg $ text "Skipping binding: " <> ppr v
        return e
      else checkAndTransform guts (Set.fromList vs) debug v e
      ) bs
    when debug $ putMsg $ "Plugin | result of transformation: " <> ppr es'
    return (Rec (zip vs es'))
  else return b
strictify opts guts b@(NonRec v e) = do
    let debug = debugMode opts
    when debug $ putMsg $ text "Processing binding: " <> ppr v <> text " | Non-recursive binding"
    when debug $ putMsg $ text "Expr: " <> ppr e
    processCore <- shouldProcessCore guts v
    if not processCore then do
      when debug $ putMsg $ text "Skipping binding: " <> ppr v
      return b
    else do
      transformed <- checkAndTransform guts Set.empty debug v e
      when debug $ putMsg $ "Plugin | result of transformation: " <> ppr transformed
      return $ NonRec v transformed

checkAndTransform :: ModGuts -> Set Var -> Bool -> Var -> CoreExpr -> CoreM CoreExpr
checkAndTransform guts recursiveSet debug v e = do
  when debug $ putMsg $ text "Processing binding: " <> ppr v
  when debug $ putMsg $ text "Expr: " <> ppr e
  allowRec <- allowRecursion guts v
  singleTick <- toSingleTick e
  when debug $ putMsg $ text "Single-tick: " <> ppr singleTick
  lazy <- allowLazyData guts v
  when (not lazy) $ checkStrictData (SCxt (nameSrcSpan $ getName v)) singleTick
  when debug $ putMsg $ text "Strict single-tick: " <> ppr singleTick
  checkExpr CheckExpr{ recursiveSet = recursiveSet, oldExpr = e,
                        verbose = debug,
                        allowRecExp = allowRec} singleTick
  transform singleTick

getModuleAnnotations :: Data a => ModGuts -> [a]
getModuleAnnotations guts = anns'
  where anns = filter (\a-> case ann_target a of
                         ModuleTarget m -> m == (mg_module guts)
                         _ -> False) (mg_anns guts)
        anns' = mapMaybe (fromSerialized deserializeWithData . ann_value) anns




allowLazyData :: ModGuts -> CoreBndr -> CoreM Bool
allowLazyData guts bndr = do
  l <- annotationsOn guts bndr :: CoreM [WidgetRattus]
  return (AllowLazyData `elem` l)

allowRecursion :: ModGuts -> CoreBndr -> CoreM Bool
allowRecursion guts bndr = do
  l <- annotationsOn guts bndr :: CoreM [WidgetRattus]
  return (AllowRecursion `elem` l)

expectError :: ModGuts -> CoreBndr -> CoreM Bool
expectError guts bndr = do
  l <- annotationsOn guts bndr :: CoreM [InternalAnn]
  return $ ExpectError `elem` l


shouldProcessCore :: ModGuts -> CoreBndr -> CoreM Bool
shouldProcessCore guts bndr = do
  expectScopeError <- expectError guts bndr
  return (userFunction bndr && not expectScopeError)

annotationsOn :: (Data a) => ModGuts -> CoreBndr -> CoreM [a]
annotationsOn guts bndr = do
  (_,anns)  <- getAnnotations deserializeWithData guts
  return $
    lookupWithDefaultUFM anns [] (varName bndr) ++
    getModuleAnnotations guts
