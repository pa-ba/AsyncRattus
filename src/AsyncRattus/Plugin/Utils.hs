{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}

module AsyncRattus.Plugin.Utils (
  printMessage,
  Severity(..),
  isRattModule,
  adv'Var,
  select'Var,
  bigDelay,
  inputValueVar,
  extractClockVar,
  unionVar,
  isGhcModule,
  getNameModule,
  isStable,
  isStrict,
  isTemporal,
  userFunction,
  typeClassFunction,
  getVar,
  getMaybeVar,
  getModuleFS,
  isVar,
  isType,
  mkSysLocalFromVar,
  mkSysLocalFromExpr,
  fromRealSrcSpan,
  noLocationInfo,
  mkAlt,
  getAlt,
  splitForAllTys')
  where

#if __GLASGOW_HASKELL__ >= 908
import GHC.Types.Error (ResolvedDiagnosticReason (..))
#endif

#if __GLASGOW_HASKELL__ >= 906
import GHC.Builtin.Types.Prim
import GHC.Tc.Utils.TcType
#endif
#if __GLASGOW_HASKELL__ >= 904
import qualified GHC.Data.Strict as Strict
import Control.Concurrent.MVar (readMVar)
#else
import Data.IORef (readIORef)
#endif  
import GHC.Utils.Logger
import GHC.Plugins
import GHC.Utils.Error hiding (errorMsg)
import GHC.Utils.Monad


import GHC.Types.Name.Cache (NameCache(nsNames), lookupOrigNameCache, OrigNameCache)
import qualified GHC.Types.Name.Occurrence as Occurrence
import GHC.Types.TyThing

import Prelude hiding ((<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Char
import Data.Maybe


getMaybeVar :: CoreExpr -> Maybe Var
getMaybeVar (App e e')
  | isType e' || not  (tcIsLiftedTypeKind (typeKind (exprType e'))) = getMaybeVar e
  | otherwise = Nothing
getMaybeVar (Cast e _) = getMaybeVar e
getMaybeVar (Tick _ e) = getMaybeVar e
getMaybeVar (Var v) = Just v
getMaybeVar _ = Nothing

getVar :: CoreExpr -> Var
getVar = fromJust . getMaybeVar

isVar :: CoreExpr -> Bool
isVar = isJust . getMaybeVar

isType Type {} = True
isType (App e _) = isType e
isType (Cast e _) = isType e
isType (Tick _ e) = isType e
isType _ = False

#if __GLASGOW_HASKELL__ >= 906
isFunTyCon = isArrowTyCon
repSplitAppTys = splitAppTysNoView
#endif
 

printMessage :: (HasDynFlags m, MonadIO m, HasLogger m) =>
                Severity -> SrcSpan -> SDoc -> m ()
printMessage sev loc doc = do
#if __GLASGOW_HASKELL__ >= 908
  logger <- getLogger
  liftIO $ putLogMsg logger (logFlags logger)
    (MCDiagnostic sev (if sev == SevError then ResolvedDiagnosticReason ErrorWithoutFlag else ResolvedDiagnosticReason WarningWithoutFlag) Nothing) loc doc
#elif __GLASGOW_HASKELL__ >= 906
  logger <- getLogger
  liftIO $ putLogMsg logger (logFlags logger)
    (MCDiagnostic sev (if sev == SevError then ErrorWithoutFlag else WarningWithoutFlag) Nothing) loc doc
#elif __GLASGOW_HASKELL__ >= 904
  logger <- getLogger
  liftIO $ putLogMsg logger (logFlags logger)
    (MCDiagnostic sev (if sev == SevError then ErrorWithoutFlag else WarningWithoutFlag)) loc doc
#else
   dflags <- getDynFlags
   logger <- getLogger
   liftIO $ putLogMsg logger dflags NoReason sev loc doc
#endif

instance Ord FastString where
   compare = uniqCompareFS

{-
******************************************************
*             Extracting variables                   *
******************************************************
-}


origNameCache :: CoreM OrigNameCache
origNameCache = do
  hscEnv <- getHscEnv
#if __GLASGOW_HASKELL__ >= 904
  let nameCache = hsc_NC hscEnv
  liftIO $ readMVar (nsNames nameCache)
#else
  nameCache <- liftIO $ readIORef (hsc_NC hscEnv)
  return $ nsNames nameCache
#endif


getNamedThingFromModuleAndOccName :: String -> OccName -> CoreM TyThing
getNamedThingFromModuleAndOccName moduleName occName = do
  origNameCache <- origNameCache
  case filter ((moduleName ==) . unpackFS . getModuleFS) (moduleEnvKeys origNameCache) of
    mod : _ -> lookupThing $ fromJust $ lookupOrigNameCache origNameCache mod occName
    _ -> error ( ("internal error: cannot find module " ++ moduleName ++ "; " ++ show (map (unpackFS . getModuleFS) $ moduleEnvKeys origNameCache)))

getVarFromModule :: String -> String -> CoreM Var
getVarFromModule moduleName = fmap tyThingId . getNamedThingFromModuleAndOccName moduleName . mkOccName Occurrence.varName

getTyConFromModule :: String -> String -> CoreM TyCon
getTyConFromModule moduleName = fmap tyThingTyCon . getNamedThingFromModuleAndOccName moduleName . mkOccName Occurrence.tcName

adv'Var :: CoreM Var
adv'Var = getVarFromModule "AsyncRattus.InternalPrimitives" "adv'"

select'Var :: CoreM Var
select'Var = getVarFromModule "AsyncRattus.InternalPrimitives" "select'"

bigDelay :: CoreM Var
bigDelay = getVarFromModule "AsyncRattus.InternalPrimitives" "Delay"

inputValueVar :: CoreM TyCon
inputValueVar = getTyConFromModule "AsyncRattus.InternalPrimitives" "InputValue"

extractClockVar :: CoreM Var
extractClockVar = getVarFromModule "AsyncRattus.InternalPrimitives" "extractClock"

unionVar :: CoreM Var
unionVar = getVarFromModule "AsyncRattus.InternalPrimitives" "clockUnion"

rattModules :: Set FastString
rattModules = Set.fromList ["AsyncRattus.InternalPrimitives","AsyncRattus.Channels"]

getModuleFS :: Module -> FastString
getModuleFS = moduleNameFS . moduleName

isRattModule :: FastString -> Bool
isRattModule = (`Set.member` rattModules)

isGhcModule :: FastString -> Bool
isGhcModule = (== "GHC.Types")

getNameModule :: NamedThing a => a -> Maybe (FastString, FastString)
getNameModule v = do
  let name = getName v
  mod <- nameModule_maybe name
  return (getOccFS name,moduleNameFS (moduleName mod))


-- | The set of stable built-in types.
ghcStableTypes :: Set FastString
ghcStableTypes = Set.fromList ["Word","Int","Bool","Float","Double","Char", "IO"]

isGhcStableType :: FastString -> Bool
isGhcStableType = (`Set.member` ghcStableTypes)


newtype TypeCmp = TC Type

instance Eq TypeCmp where
  (TC t1) == (TC t2) = eqType t1 t2

instance Ord TypeCmp where
  compare (TC t1) (TC t2) = nonDetCmpType t1 t2

isTemporal :: Type -> Bool
isTemporal t = isTemporalRec 0 Set.empty t


isTemporalRec :: Int -> Set TypeCmp -> Type -> Bool
isTemporalRec d _ _ | d == 100 = False
isTemporalRec _ pr t | Set.member (TC t) pr = False
isTemporalRec d pr t = do
  let pr' = Set.insert (TC t) pr
  case splitTyConApp_maybe t of
    Nothing -> False
    Just (con,args) ->
      case getNameModule con of
        Nothing -> False
        Just (name,mod)
          -- If it's a Rattus type constructor check if it's a box
          | isRattModule mod && (name == "Box" || name == "O") -> True
          | isFunTyCon con -> or (map (isTemporalRec (d+1) pr') args)
          | isAlgTyCon con ->
            case algTyConRhs con of
              DataTyCon {data_cons = cons} -> or (map check cons)
                where check con = case dataConInstSig con args of
                        (_, _,tys) -> or (map (isTemporalRec (d+1) pr') tys)
              _ -> or (map (isTemporalRec (d+1) pr') args)
        _ -> False


-- | Check whether the given type is stable. This check may use
-- 'Stable' constraints from the context.

isStable :: Set Var -> Type -> Bool
isStable c t = isStableRec c 0 Set.empty t

-- | Check whether the given type is stable. This check may use
-- 'Stable' constraints from the context.

isStableRec :: Set Var -> Int -> Set TypeCmp -> Type -> Bool
-- To prevent infinite recursion (when checking recursive types) we
-- keep track of previously checked types. This, however, is not
-- enough for non-regular data types. Hence we also have a counter.
isStableRec _ d _ _ | d == 100 = True
isStableRec _ _ pr t | Set.member (TC t) pr = True
isStableRec c d pr t = do
  let pr' = Set.insert (TC t) pr
  case splitTyConApp_maybe t of
    Nothing -> case getTyVar_maybe t of
      Just v -> -- if it's a type variable, check the context
        v `Set.member` c
      Nothing -> False
    Just (con,args) ->
      case getNameModule con of
        Nothing -> False
        Just (name,mod)
          | mod == "GHC.Num.Integer" && name == "Integer" -> True
          | mod == "Data.Text.Internal" && name == "Text" -> True
          -- If it's a Rattus type constructor check if it's a box
          | isRattModule mod && (name == "Box" || name == "Chan") -> True
            -- If its a built-in type check the set of stable built-in types
          | isGhcModule mod -> isGhcStableType name
          {- deal with type synonyms (does not seem to be necessary (??))
           | Just (subst,ty,[]) <- expandSynTyCon_maybe con args ->
             isStableRec c (d+1) pr' (substTy (extendTvSubstList emptySubst subst) ty) -}
          | isAlgTyCon con ->
            case algTyConRhs con of
              DataTyCon {data_cons = cons, is_enum = enum}
                | enum -> True
                | all hasStrictArgs cons ->
                  and  (map check cons)
                | otherwise -> False
                where check con = case dataConInstSig con args of
                        (_, _,tys) -> and (map (isStableRec c (d+1) pr') tys)
              TupleTyCon {} -> null args
              _ -> False
        _ -> False



isStrict :: Type -> Bool
isStrict t = isStrictRec 0 Set.empty t

splitForAllTys' :: Type -> ([TyCoVar], Type)
splitForAllTys' = splitForAllTyCoVars

-- | Check whether the given type is stable. This check may use
-- 'Stable' constraints from the context.

isStrictRec :: Int -> Set TypeCmp -> Type -> Bool
-- To prevent infinite recursion (when checking recursive types) we
-- keep track of previously checked types. This, however, is not
-- enough for non-regular data types. Hence we also have a counter.
isStrictRec d _ _ | d == 100 = True
isStrictRec _ pr t | Set.member (TC t) pr = True
isStrictRec d pr t = do
  let pr' = Set.insert (TC t) pr
  let (_,t') = splitForAllTys' t
  let (c, tys) = repSplitAppTys t'
  if isJust (getTyVar_maybe c) then and (map (isStrictRec (d+1) pr') tys)
  else  case splitTyConApp_maybe t' of
    Nothing -> isJust (getTyVar_maybe t)
    Just (con,args) ->
      case getNameModule con of
        Nothing -> False
        Just (name,mod)
          | mod == "GHC.Num.Integer" && name == "Integer" -> True
          | mod == "Data.Text.Internal" && name == "Text" -> True
          | mod == "GHC.IORef" && name == "IORef" -> True
          | mod == "GHC.MVar" && name == "MVar" -> True
          -- If it's a Rattus type constructor check if it's a box
          | isRattModule mod && (name == "Box" || name == "Chan" || name == "O" || name == "Output") -> True
            -- If its a built-in type check the set of stable built-in types
          | isGhcModule mod -> isGhcStableType name
          {- deal with type synonyms (does not seem to be necessary (??))
           | Just (subst,ty,[]) <- expandSynTyCon_maybe con args ->
             isStrictRec c (d+1) pr' (substTy (extendTvSubstList emptySubst subst) ty) -}
          | isFunTyCon con -> True
          | isAlgTyCon con ->
            case algTyConRhs con of
              DataTyCon {data_cons = cons, is_enum = enum}
                | enum -> True
                | all hasStrictArgs cons ->
                  and  (map check cons)
                | otherwise -> False
                where check con = case dataConInstSig con args of
                        (_, _,tys) -> and (map (isStrictRec (d+1) pr') tys)
              TupleTyCon {} -> null args
              NewTyCon {nt_rhs = ty} -> isStrictRec (d+1) pr' ty
              _ -> False
          | otherwise -> False





hasStrictArgs :: DataCon -> Bool
hasStrictArgs con = all isBanged (dataConImplBangs con)

userFunction :: Var -> Bool
userFunction v
  | typeClassFunction v = True
  | otherwise = 
    case getOccString (getName v) of
      (c : _)
        | isUpper c || c == '$' || c == ':' -> False
        | otherwise -> True
      _ -> False

typeClassFunction :: Var -> Bool
typeClassFunction v =
  case getOccString (getName v) of
    ('$' : 'c' : _) -> True
    ('$' : 'f' : _) -> True
    _ -> False

mkSysLocalFromVar :: MonadUnique m => FastString -> Var -> m Id
mkSysLocalFromVar lit v = mkSysLocalM lit (varMult v) (varType v)
 
mkSysLocalFromExpr :: MonadUnique m => FastString -> CoreExpr -> m Id
mkSysLocalFromExpr lit e = mkSysLocalM lit oneDataConTy (exprType e)
 
 
fromRealSrcSpan :: RealSrcSpan -> SrcSpan
#if __GLASGOW_HASKELL__ >= 904
fromRealSrcSpan span = RealSrcSpan span Strict.Nothing
#else
fromRealSrcSpan span = RealSrcSpan span Nothing
#endif

instance Ord SrcSpan where
  compare (RealSrcSpan s _) (RealSrcSpan t _) = compare s t
  compare RealSrcSpan{} _ = LT
  compare _ _ = GT

noLocationInfo :: SrcSpan
noLocationInfo = UnhelpfulSpan UnhelpfulNoLocationInfo

mkAlt c args e = Alt c args e
getAlt (Alt c args e) = (c, args, e)
