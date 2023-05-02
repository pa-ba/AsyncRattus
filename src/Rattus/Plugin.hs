{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}


-- | The plugin to make it all work.

module Rattus.Plugin (plugin, Rattus(..)) where
import Rattus.Plugin.StableSolver
import Rattus.Plugin.ScopeCheck
import Rattus.Plugin.Strictify
import Rattus.Plugin.SingleTick
import Rattus.Plugin.CheckSingleTick
import Rattus.Plugin.Utils
import Rattus.Plugin.Annotation
import Rattus.Plugin.Transform

import Prelude hiding ((<>))

import Control.Monad
import Data.Maybe
import Data.Data hiding (tyConName)
import qualified Data.Set as Set

#if __GLASGOW_HASKELL__ >= 900
import GHC.Plugins
import GHC.Tc.Types
#else
import GhcPlugins
import TcRnTypes
#endif

-- | Use this to enable Rattus' plugin, either by supplying the option
-- @-fplugin=Rattus.Plugin@ directly to GHC. or by including the
-- following pragma in each source file:
-- 
-- > {-# OPTIONS -fplugin=Rattus.Plugin #-}
plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install,
  pluginRecompile = purePlugin,
  typeCheckResultAction = typechecked,
  tcPlugin = tcStable
  }


data Options = Options {debugMode :: Bool}

typechecked :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
typechecked _ _ env = checkAll env >> return env

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opts todo = return (strPass : todo)
    where strPass = CoreDoPluginPass "Rattus strictify" (strictifyProgram Options{debugMode = dmode})
          dmode = "debug" `elem` opts

-- | Apply the following operations to all Rattus definitions in the
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
  expectCheckError <- expectCoreError guts v
  singleTick <- toSingleTick e
  when debug $ putMsg $ text "Single-tick: " <> ppr singleTick
  lazy <- allowLazyData guts v
  strict <- strictifyExpr (SCxt (nameSrcSpan $ getName v) (not lazy)) singleTick
  when debug $ putMsg $ text "Strict single-tick: " <> ppr strict
  checkExpr CheckExpr{ recursiveSet = recursiveSet, oldExpr = e,
                        expectError = expectCheckError, verbose = debug,
                        allowRecExp = allowRec} strict
  if expectCheckError
  then return strict -- don't attempt to transform an ill-typed program
  else transform strict

getModuleAnnotations :: Data a => ModGuts -> [a]
getModuleAnnotations guts = anns'
  where anns = filter (\a-> case ann_target a of
                         ModuleTarget m -> m == (mg_module guts)
                         _ -> False) (mg_anns guts)
        anns' = mapMaybe (fromSerialized deserializeWithData . ann_value) anns




allowLazyData :: ModGuts -> CoreBndr -> CoreM Bool
allowLazyData guts bndr = do
  l <- annotationsOn guts bndr :: CoreM [Rattus]
  return (AllowLazyData `elem` l)

allowRecursion :: ModGuts -> CoreBndr -> CoreM Bool
allowRecursion guts bndr = do
  l <- annotationsOn guts bndr :: CoreM [Rattus]
  return (AllowRecursion `elem` l)

expectCoreError :: ModGuts -> CoreBndr -> CoreM Bool
expectCoreError guts bndr = do
  l <- annotationsOn guts bndr :: CoreM [InternalAnn]
  return $ ExpectCoreError `elem` l

expectTcError :: ModGuts -> CoreBndr -> CoreM Bool
expectTcError guts bndr = do
  l <- annotationsOn guts bndr :: CoreM [InternalAnn]
  return $ ExpectTcError `elem` l

shouldProcessCore :: ModGuts -> CoreBndr -> CoreM Bool
shouldProcessCore guts bndr = do
  l <- annotationsOn guts bndr :: CoreM [Rattus]
  expectTcError <- expectTcError guts bndr
  return (Rattus `elem` l && notElem NotRattus l && userFunction bndr && not expectTcError)

annotationsOn :: (Data a) => ModGuts -> CoreBndr -> CoreM [a]
annotationsOn guts bndr = do
#if __GLASGOW_HASKELL__ >= 900
  (_,anns)  <- getAnnotations deserializeWithData guts
  return $
    lookupWithDefaultUFM anns [] (varName bndr) ++
    getModuleAnnotations guts
#else    
  anns <- getAnnotations deserializeWithData guts
  return $
    lookupWithDefaultUFM anns [] (varUnique bndr) ++
    getModuleAnnotations guts
#endif
