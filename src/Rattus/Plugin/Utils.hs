{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE InstanceSigs #-}

module Rattus.Plugin.Utils (
  printMessage,
  Severity(..),
  isRattModule,
  isGhcModule,
  getNameModule,
  isStable,
  isStrict,
  isTemporal,
  userFunction,
  isVar,
  isType,
  mkSysLocalFromVar,
  mkSysLocalFromExpr,
  fromRealSrcSpan,
  noLocationInfo,
  mkAlt,
  getAlt,
  showTree,
  splitForAllTys')
  where
#if __GLASGOW_HASKELL__ >= 906
import GHC.Builtin.Types.Prim
import GHC.Tc.Utils.TcType
#endif
#if __GLASGOW_HASKELL__ >= 904
import qualified GHC.Data.Strict as Strict
#endif  
#if __GLASGOW_HASKELL__ >= 902
import GHC.Utils.Logger
#endif

#if __GLASGOW_HASKELL__ >= 900
import GHC.Plugins
import GHC.Utils.Error
import GHC.Utils.Monad
#else
import GhcPlugins
import ErrUtils
import MonadUtils
#endif



import Prelude hiding ((<>))

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Char
import Data.Maybe

isVar :: CoreExpr -> Bool
isVar (App e e')
  | isType e' || not  (tcIsLiftedTypeKind(typeKind (exprType e'))) = isVar e
  | otherwise = False
isVar (Cast e _) = isVar e
isVar (Tick _ e) = isVar e
isVar (Var _) = True
isVar _ = False

isType Type {} = True
isType (App e _) = isType e
isType (Cast e _) = isType e
isType (Tick _ e) = isType e
isType _ = False


#if __GLASGOW_HASKELL__ >= 906
isFunTyCon = isArrowTyCon
repSplitAppTys = splitAppTysNoView
#endif

#if __GLASGOW_HASKELL__ >= 902
printMessage :: (HasDynFlags m, MonadIO m, HasLogger m) =>
                Severity -> SrcSpan -> SDoc -> m ()
#else
printMessage :: (HasDynFlags m, MonadIO m) =>
                Severity -> SrcSpan -> MsgDoc -> m ()
#endif
printMessage sev loc doc = do
#if __GLASGOW_HASKELL__ >= 906
  logger <- getLogger
  liftIO $ putLogMsg logger (logFlags logger)
    (MCDiagnostic sev (if sev == SevError then ErrorWithoutFlag else WarningWithoutFlag) Nothing) loc doc
#elif __GLASGOW_HASKELL__ >= 904
  logger <- getLogger
  liftIO $ putLogMsg logger (logFlags logger)
    (MCDiagnostic sev (if sev == SevError then ErrorWithoutFlag else WarningWithoutFlag)) loc doc
#elif __GLASGOW_HASKELL__ >= 902
  dflags <- getDynFlags
  logger <- getLogger
  liftIO $ putLogMsg logger dflags NoReason sev loc doc
#elif __GLASGOW_HASKELL__ >= 900  
  dflags <- getDynFlags
  liftIO $ putLogMsg dflags NoReason sev loc doc
#else
  dflags <- getDynFlags
  let sty = case sev of
              SevError   -> defaultErrStyle dflags
              SevWarning -> defaultErrStyle dflags
              SevDump    -> defaultDumpStyle dflags
              _          -> defaultUserStyle dflags
  liftIO $ putLogMsg dflags NoReason sev loc sty doc
#endif

#if __GLASGOW_HASKELL__ >= 902
instance Ord FastString where
  compare = uniqCompareFS
#endif

rattModules :: Set FastString
rattModules = Set.fromList ["Rattus.Internal","Rattus.Primitives"
                           ,"Rattus.Stable", "Rattus.Arrow"]

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
ghcStableTypes = Set.fromList ["Int","Bool","Float","Double","Char", "IO"]

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
          -- If it's a Rattus type constructor check if it's a box
          | isRattModule mod && name == "Box" -> True
            -- If its a built-in type check the set of stable built-in types
          | isGhcModule mod -> isGhcStableType name 
          {- deal with type synonyms (does not seem to be necessary (??))
           | Just (subst,ty,[]) <- expandSynTyCon_maybe con args ->
             isStableRec c (d+1) pr' (substTy (extendTvSubstList emptySubst subst) ty) -}
          | isAlgTyCon con -> 
            case algTyConRhs con of
              DataTyCon {data_cons = cons, is_enum = enum}
                | enum -> True
                | and $ concatMap (map isSrcStrict'
                                   . dataConSrcBangs) $ cons ->
                  and  (map check cons)
                | otherwise -> False
                where check con = case dataConInstSig con args of
                        (_, _,tys) -> and (map (isStableRec c (d+1) pr') tys)
              TupleTyCon {} -> null args
              _ -> False
        _ -> False



isStrict :: Type -> Bool
isStrict t = isStrictRec 0 Set.empty t

#if __GLASGOW_HASKELL__ >= 902
splitForAllTys' :: Type -> ([TyCoVar], Type)
splitForAllTys' = splitForAllTyCoVars
#else
splitForAllTys' = splitForAllTys
#endif

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
          -- If it's a Rattus type constructor check if it's a box
          | isRattModule mod && (name == "Box" || name == "O") -> True
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
                | and $ (map (areSrcStrict args)) $ cons ->
                  and  (map check cons)
                | otherwise -> False
                where check con = case dataConInstSig con args of
                        (_, _,tys) -> and (map (isStrictRec (d+1) pr') tys)
              TupleTyCon {} -> null args
              _ -> False
          | otherwise -> False
            




areSrcStrict :: [Type] -> DataCon -> Bool
areSrcStrict args con = and (zipWith check tys (dataConSrcBangs con))
  where (_, _,tys) = dataConInstSig con args 
        check _ b = isSrcStrict' b

isSrcStrict' :: HsSrcBang -> Bool
isSrcStrict' (HsSrcBang _ _ SrcStrict) = True
isSrcStrict' _ = False


userFunction :: Var -> Bool
userFunction v =
  case getOccString (getName v) of
    (c : _)
      | isUpper c || c == '$' || c == ':' -> False
      | otherwise -> True
    _ -> False



mkSysLocalFromVar :: MonadUnique m => FastString -> Var -> m Id
#if __GLASGOW_HASKELL__ >= 900
mkSysLocalFromVar lit v = mkSysLocalM lit (varMult v) (varType v)
#else
mkSysLocalFromVar lit v = mkSysLocalM lit (varType v)
#endif

mkSysLocalFromExpr :: MonadUnique m => FastString -> CoreExpr -> m Id
#if __GLASGOW_HASKELL__ >= 900
mkSysLocalFromExpr lit e = mkSysLocalM lit oneDataConTy (exprType e)
#else
mkSysLocalFromExpr lit e = mkSysLocalM lit (exprType e)
#endif


fromRealSrcSpan :: RealSrcSpan -> SrcSpan
#if __GLASGOW_HASKELL__ >= 904
fromRealSrcSpan span = RealSrcSpan span Strict.Nothing
#elif __GLASGOW_HASKELL__ >= 900
fromRealSrcSpan span = RealSrcSpan span Nothing
#else
fromRealSrcSpan span = RealSrcSpan span
#endif


#if __GLASGOW_HASKELL__ >= 900
instance Ord SrcSpan where
  compare (RealSrcSpan s _) (RealSrcSpan t _) = compare s t
  compare RealSrcSpan{} _ = LT
  compare _ _ = GT
#endif

noLocationInfo :: SrcSpan
#if __GLASGOW_HASKELL__ >= 900
noLocationInfo = UnhelpfulSpan UnhelpfulNoLocationInfo
#else         
noLocationInfo = UnhelpfulSpan "<no location info>"
#endif



#if __GLASGOW_HASKELL__ >= 902
mkAlt c args e = Alt c args e
getAlt (Alt c args e) = (c, args, e)
#else
mkAlt c args e = (c, args, e)
getAlt alt = alt
#endif


showOutputable :: (Outputable a) => a -> String
showOutputable = showSDocUnsafe . ppr

indent :: Int -> String -> String
indent i s = replicate (2 * i) ' ' ++ s

showTree :: Expr Var -> String
showTree = showTree' 0

showTree' :: Int -> Expr Var -> String
showTree' i (Var id) = indent i "Var {" ++ showOutputable (varName id) ++ "}"
showTree' i (Lit literal) = indent i "Literal {" ++ showOutputable literal ++ "}"
showTree' i (App e e') = indent i "App {\n(" ++ showTree' (i+1) e ++ ")\n(" ++ showTree' (i+1) e' ++ "\n" ++ indent i ")}"
showTree' i (Lam b e) = indent i "Lam {\n" ++ showVar (i+1) b ++ "\n" ++ showTree' (i+1) e ++ "\n" ++ indent i "}"
showTree' i (Let b e) = indent i "Let {\n" ++ showBindVar (i+1) b ++ "\n" ++ showTree' (i+1) e ++ "\n" ++ indent i "}"
showTree' i (Case e b t as) = indent i "Case {\n" ++ showTree' (i+1) e ++ "\n" ++ showVar (i+1) b ++ "\n" ++ showType (i+1) t ++ "\n" ++ indent i "AltList {\n" ++ showAltList (i+1) as ++ indent i "}"
showTree' _ e = showOutputable e

showAltList :: Int -> [Alt Var] -> String
showAltList i = foldl (\s a -> indent i s ++ "\n" ++ showAlt (i+1) a) ""

showList2 :: (Show a) => [a] -> String
showList2 = foldl (\s a -> s ++ show a) ""

showAlt :: Int -> Alt Var -> String
showAlt i (Alt con bindings rhs) = "Alt {(" ++ showOutputable con ++ ") (BINDINGS) (" ++ showTree' (i+1) rhs ++ ")}"

showVar :: Int -> Var -> String
showVar i = indent i . getOccString

showBindVar :: Int -> Bind Var -> String
showBindVar i (NonRec b e) = indent i "NonRec {(" ++ showVar (i+1) b ++ indent i ") (" ++ showTree' (i+1) e ++ indent i ")}"
showBindVar i (Rec bindings) = indent i "Rec {" ++ foldl (\s (b, e) -> s ++ ("(" ++ showVar (i+1) b ++ ") (" ++ showTree' (i+1) e ++ ")")) "" bindings ++ indent i "}"

showType :: Int -> Type -> String
showType i = indent i . showSDocUnsafe . ppr
