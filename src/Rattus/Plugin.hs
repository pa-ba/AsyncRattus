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
  --typeCheckResultAction = typechecked,
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
  tr <- liftM or (mapM (shouldTransform guts . fst) bs)
  if tr then do
    let vs = map fst bs
    es' <- mapM (\ (v,e) -> do
                    singleTick <- toSingleTick e
                    lazy <- allowLazyData guts v
                    allowRec <- allowRecursion guts v
                    strict <- strictifyExpr (SCxt (nameSrcSpan $ getName v) (not lazy)) singleTick
                    ok <- checkExpr CheckExpr{ recursiveSet = Set.fromList vs, oldExpr = e,
                                         fatalError = True, verbose = debugMode opts,
                                         allowRecExp = allowRec} strict
                    if ok
                      then transform strict
                      else error "Rattus: Ill-typed program") bs
    return (Rec (zip vs es'))
  else return b
strictify opts guts b@(NonRec v e) = do
    tr <- shouldTransform guts v
    if tr then do
      -- liftIO $ putStrLn "-------- old --------"
      -- liftIO $ putStrLn (showSDocUnsafe (ppr e))
      singleTick <- toSingleTick e
      -- liftIO $ putStrLn "-------- new --------"
      -- liftIO $ putStrLn (showSDocUnsafe (ppr e'))
      lazy <- allowLazyData guts v
      allowRec <- allowRecursion guts v
      strict <- strictifyExpr (SCxt (nameSrcSpan $ getName v) (not lazy)) singleTick
      ok <- checkExpr CheckExpr{ recursiveSet = Set.empty, oldExpr = e,
                           fatalError = True, verbose = debugMode opts,
                           allowRecExp = allowRec } strict
      if ok
      then do
        transformed <- transform strict
        return $ NonRec v transformed
      else
        error "Rattus: Ill-typed program"
    else return b

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


shouldTransform :: ModGuts -> CoreBndr -> CoreM Bool
shouldTransform guts bndr = do
  l <- annotationsOn guts bndr :: CoreM [Rattus]
  l' <- annotationsOn guts bndr :: CoreM [InternalAnn]
  return ((Rattus `elem` l && not (NotRattus `elem` l) && userFunction bndr) && not (ExpectError `elem` l'))

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
