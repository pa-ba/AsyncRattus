{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}


-- | This module implements a constraint solver plugin for the
-- 'Stable' type class.

module AsyncRattus.Plugin.StableSolver (tcStable) where

import AsyncRattus.Plugin.Utils
    ( getNameModule, isRattModule, isStable )

import Prelude hiding ((<>))

#if __GLASGOW_HASKELL__ >= 900
import GHC.Plugins
  (Type, Var, CommandLineOption,tyConSingleDataCon,
   mkCoreConApps,getTyVar_maybe)
import GHC.Core
import GHC.Tc.Types.Evidence
import GHC.Core.Class
import GHC.Tc.Types
#else
import GhcPlugins
  (Type, Var, CommandLineOption,tyConSingleDataCon,
   mkCoreConApps,getTyVar_maybe)
import CoreSyn
import TcEvidence
import Class
import TcRnTypes
#endif

#if __GLASGOW_HASKELL__ >= 900
import GHC.Tc.Types.Constraint
#elif __GLASGOW_HASKELL__ >= 810
import Constraint
#endif

import Data.Set (Set)
import qualified Data.Set as Set
#if __GLASGOW_HASKELL__ >= 904
import GHC.Types.Unique.FM
#endif



-- | Constraint solver plugin for the 'Stable' type class.
tcStable :: [CommandLineOption] -> Maybe TcPlugin
tcStable _ = Just $ TcPlugin
  { tcPluginInit = return ()
  , tcPluginSolve = \ () -> stableSolver
  , tcPluginStop = \ () -> return ()
#if __GLASGOW_HASKELL__ >= 904
  , tcPluginRewrite = \ () -> emptyUFM
#endif
  }


wrap :: Class -> Type -> EvTerm
wrap cls ty = EvExpr appDc
  where
    tyCon = classTyCon cls
    dc = tyConSingleDataCon tyCon
    appDc = mkCoreConApps dc [Type ty]

solveStable :: Set Var -> (Type, (Ct,Class)) -> Maybe (EvTerm, Ct)
solveStable c (ty,(ct,cl))
  | isStable c ty = Just (wrap cl ty, ct)
  | otherwise = Nothing

#if __GLASGOW_HASKELL__ >= 904
stableSolver :: EvBindsVar -> [Ct] -> [Ct] -> TcPluginM TcPluginSolveResult
stableSolver _ given wanted = do
#else
stableSolver :: [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
stableSolver given _derived wanted = do
#endif

  let chSt = concatMap filterCt wanted
  let haveSt = Set.fromList $ concatMap (filterTypeVar . fst) $ concatMap filterCt given
  case mapM (solveStable haveSt) chSt of
    Just evs -> return $ TcPluginOk evs []
    Nothing -> return $ TcPluginOk [] []

  where
#if __GLASGOW_HASKELL__ >= 908
        filterCt ct@(CDictCan (DictCt {di_cls = cl, di_tys = [ty]}))
#else
        filterCt ct@(CDictCan {cc_class = cl, cc_tyargs = [ty]})
#endif
          = case getNameModule cl of
                Just (name,mod)
                  | isRattModule mod && name == "Stable" -> [(ty,(ct,cl))]
                _ -> []
        filterCt _ = []
        filterTypeVar ty = case getTyVar_maybe ty of
          Just v -> [v]
          Nothing -> []
