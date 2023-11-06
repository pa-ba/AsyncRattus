{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}



-- | This module implements the source plugin that checks the variable
-- scope of of Async Rattus programs.

module AsyncRattus.Plugin.ScopeCheck (checkAll) where

import AsyncRattus.Plugin.Utils
import AsyncRattus.Plugin.Dependency
import AsyncRattus.Plugin.Annotation

import Control.Monad.Trans.State.Strict
import Data.IORef

import Prelude hiding ((<>))

import GHC.Parser.Annotation
import GHC.Plugins
import GHC.Tc.Types
import GHC.Data.Bag
import GHC.Tc.Types.Evidence
import GHC.Hs.Extension
import GHC.Hs.Expr
import GHC.Hs.Pat
import GHC.Hs.Binds

import Data.Graph
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)
import Data.List
import Data.List.NonEmpty (NonEmpty(..),(<|),nonEmpty)
import System.Exit
import Data.Either
import Data.Maybe

import Data.Data hiding (tyConName)

import Control.Monad

type ErrorMsg = (Severity,SrcSpan,SDoc)
type ErrorMsgsRef = IORef [ErrorMsg]

-- | The current context for scope checking
data Ctxt = Ctxt
  {
    errorMsgs :: ErrorMsgsRef,
    -- | Variables that are in scope now (i.e. occurring in the typing
    -- context but not to the left of a tick)
    current :: LCtxt,
    -- | Variables that are in the typing context, but to the left of a
    -- tick
    earlier :: Either NoTickReason (NonEmpty LCtxt),
    -- | Variables that have fallen out of scope. The map contains the
    -- reason why they have fallen out of scope.
    hidden :: Hidden,
    -- -- | Same as 'hidden' but for recursive variables.
    -- hiddenRec :: Hidden,
    -- | The current location information.
    srcLoc :: SrcSpan,
    -- | If we are in the body of a recursively defined function, this
    -- field contains the variables that are defined recursively
    -- (could be more than one due to mutual recursion or because of a
    -- recursive pattern definition) and the location of the recursive
    -- definition.
    recDef :: Maybe RecDef,
    -- | Type variables with a 'Stable' constraint attached to them.
    stableTypes :: Set Var,
    -- | A mapping from variables to the primitives that they are
    -- defined equal to. For example, a program could contain @let
    -- mydel = delay in mydel 1@, in which case @mydel@ is mapped to
    -- 'Delay'.
    primAlias :: Map Var Prim,
    -- | Allow general recursion.
    allowRecursion :: Bool}



-- | The starting context for checking a top-level definition. For
-- non-recursive definitions, the argument is @Nothing@. Otherwise, it
-- contains the recursively defined variables along with the location
-- of the recursive definition.
emptyCtxt :: ErrorMsgsRef -> Maybe (Set Var,SrcSpan) -> Bool -> Ctxt
emptyCtxt em mvar allowRec =
  Ctxt { errorMsgs = em,
         current =  Set.empty,
         earlier = Left NoDelay,
         hidden = Map.empty,
         srcLoc = noLocationInfo,
         recDef = mvar,
         primAlias = Map.empty,
         stableTypes = Set.empty,
         allowRecursion = allowRec}

-- | A local context, consisting of a set of variables.
type LCtxt = Set Var

-- | The recursively defined variables + the position where the
-- recursive definition starts
type RecDef = (Set Var, SrcSpan)




data StableReason = StableRec SrcSpan | StableBox deriving Show

-- | Indicates, why a variable has fallen out of scope.
data HiddenReason = Stabilize StableReason | FunDef | DelayApp | AdvApp | SelectApp deriving Show

-- | Indicates, why there is no tick
data NoTickReason = NoDelay | TickHidden HiddenReason deriving Show

-- | Hidden context, containing variables that have fallen out of
-- context along with the reason why they have.
type Hidden = Map Var HiddenReason

-- | The 5 primitive Asynchronous Rattus operations.
data Prim = Delay | Adv | Select | Box | Unbox deriving Show

-- | This constraint is used to pass along the context implicitly via
-- an implicit parameter.
type GetCtxt = ?ctxt :: Ctxt


type CheckM = StateT ([Maybe (Prim, SrcSpan)]) TcM

-- | This type class is implemented for each AST type @a@ for which we
-- can check whether it adheres to the scoping rules of Asynchronous Rattus.
class Scope a where
  -- | Check whether the argument is a scope correct piece of syntax
  -- in the given context.
  check :: GetCtxt => a -> CheckM Bool

-- | This is a variant of 'Scope' for syntax that can also bind
-- variables.
class ScopeBind a where
  -- | 'checkBind' checks whether its argument is scope-correct and in
  -- addition returns the the set of variables bound by it.
  checkBind :: GetCtxt => a -> CheckM (Bool,Set Var)


-- | set the current context.
setCtxt :: Ctxt -> (GetCtxt => a) -> a 
setCtxt c a = let ?ctxt = c in a


-- | modify the current context.
modifyCtxt :: (Ctxt -> Ctxt) -> (GetCtxt => a) -> (GetCtxt => a)
modifyCtxt f a =
  let newc = f ?ctxt in
  let ?ctxt = newc in a




getLocAnn' :: SrcSpanAnn' b -> SrcSpan
getLocAnn' = locA


updateLoc :: SrcSpanAnn' b -> (GetCtxt => a) -> (GetCtxt => a)
updateLoc src = modifyCtxt (\c -> c {srcLoc = getLocAnn' src})


-- | Check all definitions in the given module. If Scope errors are
-- found, the current execution is halted with 'exitFailure'.
checkAll :: TcGblEnv -> TcM ()
checkAll env = do
  let dep = dependency (tcg_binds env)
  let bindDep = filter (filterBinds (tcg_mod env) (tcg_ann_env env)) dep
  result <- mapM (checkSCC' (tcg_mod env) (tcg_ann_env env)) bindDep
  let (res,msgs) = foldl' (\(b,l) (b',l') -> (b && b', l ++ l')) (True,[]) result
  printAccErrMsgs msgs
  if res then return () else liftIO exitFailure


printAccErrMsgs :: [ErrorMsg] -> TcM ()
printAccErrMsgs msgs = mapM_ printMsg (sortOn (\(_,l,_)->l) msgs)
  where printMsg (sev,loc,doc) = printMessage sev loc doc


-- | This function checks whether a given top-level definition (either
-- a single non-recursive definition or a group of mutual recursive
-- definitions) is marked as Asynchronous Rattus code (via an annotation). In a
-- group of mutual recursive definitions, the whole group is
-- considered Asynchronous Rattus code if at least one of its constituents is
-- marked as such.
filterBinds :: Module -> AnnEnv -> SCC (LHsBindLR  GhcTc GhcTc, Set Var) -> Bool
filterBinds mod anEnv scc =
  case scc of
    (AcyclicSCC (_,vs)) -> any checkVar vs
    (CyclicSCC bs) -> any (any checkVar . snd) bs
  where checkVar :: Var -> Bool
        checkVar v =
          let anns = findAnns deserializeWithData anEnv (NamedTarget name) :: [AsyncRattus]
              annsMod = findAnns deserializeWithData anEnv (ModuleTarget mod) :: [AsyncRattus]
              name :: Name
              name = varName v
          in AsyncRattus `elem` anns || (not (NotAsyncRattus `elem` anns)  && AsyncRattus `elem` annsMod)



instance Scope a => Scope (GenLocated SrcSpan a) where
  check (L l x) =  (\c -> c {srcLoc = l}) `modifyCtxt` check x

instance Scope a => Scope (GenLocated (SrcSpanAnn' b) a) where
  check (L l x) =  updateLoc l $ check x
  
instance Scope a => Scope (Bag a) where
  check bs = fmap and (mapM check (bagToList bs))

instance Scope a => Scope [a] where
  check ls = fmap and (mapM check ls)


instance Scope (Match GhcTc (GenLocated SrcAnno (HsExpr GhcTc))) where
  check Match{m_pats=ps,m_grhss=rhs} = addVars (getBV ps) `modifyCtxt` check rhs

instance Scope (Match GhcTc (GenLocated SrcAnno (HsCmd GhcTc))) where
  check Match{m_pats=ps,m_grhss=rhs} = addVars (getBV ps) `modifyCtxt` check rhs


instance Scope (MatchGroup GhcTc (GenLocated SrcAnno (HsExpr GhcTc))) where
  check MG {mg_alts = alts} = check alts


instance Scope (MatchGroup GhcTc (GenLocated SrcAnno (HsCmd GhcTc))) where
  check MG {mg_alts = alts} = check alts


instance Scope a => ScopeBind (StmtLR GhcTc GhcTc a) where
  checkBind (LastStmt _ b _ _) =  ( , Set.empty) <$> check b
  checkBind (BindStmt _ p b) = do
    let vs = getBV p
    let c' = addVars vs ?ctxt
    r <- setCtxt c' (check b)
    return (r,vs)
  checkBind (BodyStmt _ b _ _) = ( , Set.empty) <$> check b
  checkBind (LetStmt _ bs) = checkBind bs
  checkBind ParStmt{} = notSupported "monad comprehensions"
  checkBind TransStmt{} = notSupported "monad comprehensions"
  checkBind ApplicativeStmt{} = notSupported "applicative do notation"
  checkBind RecStmt{} = notSupported "recursive do notation"

instance ScopeBind a => ScopeBind [a] where
  checkBind [] = return (True,Set.empty)
  checkBind (x:xs) = do
    (r,vs) <- checkBind x
    (r',vs') <- addVars vs `modifyCtxt` (checkBind xs)
    return (r && r',vs `Set.union` vs')

instance ScopeBind a => ScopeBind (GenLocated SrcSpan a) where
  checkBind (L l x) =  (\c -> c {srcLoc = l}) `modifyCtxt` checkBind x

instance ScopeBind a => ScopeBind (GenLocated (SrcSpanAnn' b) a) where
  checkBind (L l x) =  updateLoc l $ checkBind x

instance Scope a => Scope (GRHS GhcTc a) where
  check (GRHS _ gs b) = do
    (r, vs) <- checkBind gs
    r' <- addVars vs `modifyCtxt`  (check b)
    return (r && r')

checkRec :: GetCtxt => LHsBindLR GhcTc GhcTc -> CheckM Bool
checkRec b =  liftM2 (&&) (checkPatBind b) (check b)

checkPatBind :: GetCtxt => LHsBindLR GhcTc GhcTc -> CheckM Bool
checkPatBind (L l b) = updateLoc l $ checkPatBind' b

checkPatBind' :: GetCtxt => HsBindLR GhcTc GhcTc -> CheckM Bool
checkPatBind' PatBind{} = do
  printMessage' SevError ("(Mutual) recursive pattern binding definitions are not supported in Asynchronous Rattus")
  return False
#if __GLASGOW_HASKELL__ < 904
checkPatBind' AbsBinds {abs_binds = binds} = 
#else
checkPatBind' (XHsBindsLR AbsBinds {abs_binds = binds}) = 
#endif
  liftM and (mapM checkPatBind (bagToList binds))

checkPatBind' _ = return True


-- | Check the scope of a list of (mutual) recursive bindings. The
-- second argument is the set of variables defined by the (mutual)
-- recursive bindings
checkRecursiveBinds :: GetCtxt => [LHsBindLR GhcTc GhcTc] -> Set Var -> CheckM (Bool, Set Var)
checkRecursiveBinds bs vs = do
    res <- fmap and (mapM check' bs)
    return (res, vs)
    where check' b@(L l _) = fc (getLocAnn' l) `modifyCtxt` checkRec b
          fc l c = let
            ctxHid = either (const $ current c) (Set.union (current c) . Set.unions) (earlier c)
            in c {current = Set.empty,
                  earlier = Left (TickHidden $ Stabilize $ StableRec l),
                  hidden =  hidden c `Map.union`
                            (Map.fromSet (const (Stabilize (StableRec l))) ctxHid),
                  recDef = maybe (Just (vs,l)) (\(vs',_) -> Just (Set.union vs' vs,l)) (recDef c)
                   -- TODO fix location info of recDef (needs one location for each var)
                   }          


instance ScopeBind (SCC (GenLocated SrcSpanAnnA (HsBindLR  GhcTc GhcTc), Set Var)) where
  checkBind (AcyclicSCC (b,vs)) = (, vs) <$> check b
  checkBind (CyclicSCC bs) = checkRecursiveBinds (map fst bs) (foldMap snd bs)
  
instance ScopeBind (HsValBindsLR GhcTc GhcTc) where
  checkBind (ValBinds _ bs _) = checkBind (dependency bs)
  
  checkBind (XValBindsLR (NValBinds binds _)) = checkBind binds


instance ScopeBind (HsBindLR GhcTc GhcTc) where
  checkBind b = (, getBV b) <$> check b


-- | Compute the set of variables defined by the given Haskell binder.
getAllBV :: GenLocated l (HsBindLR GhcTc GhcTc) -> Set Var
getAllBV (L _ b) = getAllBV' b where
  getAllBV' (FunBind{fun_id = L _ v}) = Set.singleton v
#if __GLASGOW_HASKELL__ < 904
  getAllBV' (AbsBinds {abs_exports = es, abs_binds = bs}) = Set.fromList (map abe_poly es) `Set.union` foldMap getBV bs
  getAllBV' XHsBindsLR{} = Set.empty
#else
  getAllBV' (XHsBindsLR (AbsBinds {abs_exports = es, abs_binds = bs})) = Set.fromList (map abe_poly es) `Set.union` foldMap getBV bs
#endif
  getAllBV' (PatBind {pat_lhs = pat}) = getBV pat
  getAllBV' (VarBind {var_id = v}) = Set.singleton v
  getAllBV' PatSynBind{} = Set.empty


-- Check nested bindings
instance ScopeBind (RecFlag, Bag (GenLocated SrcSpanAnnA (HsBindLR GhcTc GhcTc))) where
  checkBind (NonRecursive, bs)  = checkBind $ bagToList bs
  checkBind (Recursive, bs) = checkRecursiveBinds bs' (foldMap getAllBV bs')
    where bs' = bagToList bs


instance ScopeBind (HsLocalBindsLR GhcTc GhcTc) where
  checkBind (HsValBinds _ bs) = checkBind bs
  checkBind HsIPBinds {} = notSupported "implicit parameters"
  checkBind EmptyLocalBinds{} = return (True,Set.empty)

type SrcAnno = SrcSpanAnnA
  
instance Scope (GRHSs GhcTc (GenLocated SrcAnno (HsExpr GhcTc))) where
  check GRHSs{grhssGRHSs = rhs, grhssLocalBinds = lbinds} = do
    (l,vs) <- checkBind lbinds
    r <- addVars vs `modifyCtxt` (check rhs)
    return (r && l)

instance Scope (GRHSs GhcTc (GenLocated SrcAnno (HsCmd GhcTc))) where
  check GRHSs{grhssGRHSs = rhs, grhssLocalBinds = lbinds} = do
    (l,vs) <- checkBind lbinds
    r <- addVars vs `modifyCtxt` (check rhs)
    return (r && l)

instance Show Var where
  show v = getOccString v


tickHidden :: HiddenReason -> SDoc
tickHidden FunDef = "a function definition"
tickHidden DelayApp = "a nested application of delay"
tickHidden AdvApp = "an application of adv"
tickHidden SelectApp = "an application of select"
tickHidden (Stabilize StableBox) = "an application of box"
tickHidden (Stabilize (StableRec src)) = "a nested recursive definition (at " <> ppr src <> ")"

isSelect :: GetCtxt => LHsExpr GhcTc -> Bool
isSelect e =
  case isPrimExpr e of
    Just (Select, _) -> True
    _ -> False

instance Scope (HsExpr GhcTc) where
  check (HsVar _ (L _ v))
    | Just p <- isPrim v =
        case p of
          Unbox -> return True
          _ -> printMessageCheck SevError ("Defining an alias for " <> ppr v <> " is not allowed")
    | otherwise = case getScope v of
             Hidden reason -> printMessageCheck SevError reason
             Visible -> return True
             ImplUnboxed -> return True
               -- printMessageCheck SevWarning
               --  (ppr v <> text " is an external temporal function used under delay, which may cause time leaks.")
  check (HsApp _ (L _ (HsApp _ f arg)) arg2) | isSelect f =
    case earlier ?ctxt of
      Right (er :| ers) -> do
        res <- get
        case res of
            Just _ : _ -> printMessageCheck SevError ("only one adv or select may be used in the scope of a delay.")
            Nothing : pre -> do put pre
                                b1 <- mod `modifyCtxt` check arg
                                b2 <- mod `modifyCtxt` check arg2
                                modify (Just (Select, srcLoc ?ctxt) :)
                                return $ b1 && b2
            _ -> error "Asynchronous Rattus: internal error"
        where mod c =  c{earlier = case nonEmpty ers of
                                    Nothing -> Left $ TickHidden SelectApp
                                    Just ers' -> Right ers',
                        current = er,
                        hidden = hidden ?ctxt `Map.union`
                        Map.fromSet (const SelectApp) (current ?ctxt)}
      Left NoDelay -> printMessageCheck SevError "select may only be used in the scope of a delay."
      Left (TickHidden hr) -> printMessageCheck SevError ("select may only be used in the scope of a delay. "
                        <> " There is a delay, but its scope is interrupted by " <> tickHidden hr <> ".")
  check (HsApp _ e1 e2) =
    case isPrimExpr e1 of
    Just (p,_) -> case p of
      Box -> do
        ch <- stabilize StableBox `modifyCtxt` check e2
        return ch
      Unbox -> check e2
      Delay -> do modify (Nothing :)
                  b <- (\c -> c{current = Set.empty,
                           earlier = case earlier c of
                                      Left _ -> Right (current c :| [])
                                      Right cs -> Right (current c <| cs)})
                     `modifyCtxt` check e2
                  res <- get
                  case res of
                    Nothing : _ -> printMessageCheck SevError "No adv or select found in the scope of this occurrence of delay"
                    _ : pre -> put pre >> return b
                    _ -> error "Asynchronous Rattus: internal error"
      Adv -> case earlier ?ctxt of
        Right (er :| ers) -> do
          res <- get
          case res of
            Just _ : _ -> printMessageCheck SevError ("only one adv or select may be used in the scope of a delay.")
            Nothing : pre -> do put pre
                                b <- mod `modifyCtxt` check e2
                                modify (Just (Adv,srcLoc ?ctxt) :)
                                return b
            _ -> error "Asynchronous Rattus: internal error"
          where mod c =  c{earlier = case nonEmpty ers of
                                       Nothing -> Left $ TickHidden AdvApp
                                       Just ers' -> Right ers',
                           current = er,
                           hidden = hidden ?ctxt `Map.union`
                            Map.fromSet (const AdvApp) (current ?ctxt)}
        Left NoDelay -> printMessageCheck SevError ("adv may only be used in the scope of a delay.")
        Left (TickHidden hr) -> printMessageCheck SevError ("adv may only be used in the scope of a delay. "
                            <> " There is a delay, but its scope is interrupted by " <> tickHidden hr <> ".")
      Select -> printMessageCheck SevError ("select must be fully applied")
    _ -> liftM2 (&&) (check e1)  (check e2)
  check HsUnboundVar{}  = return True
#if __GLASGOW_HASKELL__ >= 904
  check (HsPar _ _ e _) = check e
  check (HsLamCase _ _ mg) = check mg
  check HsRecSel{} = return True
  check HsTypedBracket{} = notSupported "MetaHaskell"
  check HsUntypedBracket{} = notSupported "MetaHaskell"
#else
  check HsConLikeOut{} = return True
  check HsRecFld{} = return True
  check (HsPar _ e) = check e
  check (HsLamCase _ mg) = check mg
  check HsBracket{} = notSupported "MetaHaskell"
  check (HsTick _ _ e) = check e
  check (HsBinTick _ _ _ e) = check e
  check HsRnBracketOut{} = notSupported "MetaHaskell"
  check HsTcBracketOut{} = notSupported "MetaHaskell"
#endif
#if __GLASGOW_HASKELL__ >= 904
  check (HsLet _ _ bs _ e) = do
#else
  check (HsLet _ bs e) = do
#endif
    (l,vs) <- checkBind bs
    r <- addVars vs `modifyCtxt` (check e)
    return (r && l)
         
  check HsOverLabel{} = return True
  check HsIPVar{} = notSupported "implicit parameters"
  check HsOverLit{} = return True  
  check HsLit{} = return True
  check (OpApp _ e1 e2 e3) = and <$> mapM check [e1,e2,e3]
  check (HsLam _ mg) = check mg
  check (HsCase _ e1 e2) = (&&) <$> check e1 <*> check e2
  check (SectionL _ e1 e2) = (&&) <$> check e1 <*> check e2
  check (SectionR _ e1 e2) = (&&) <$> check e1 <*> check e2
  check (ExplicitTuple _ e _) = check e
  check (NegApp _ e _) = check e
  check (ExplicitSum _ _ _ e) = check e
  check (HsMultiIf _ e) = check e
  check (ExplicitList _ e) = check e
  check HsProjection {} = return True
  check HsGetField {gf_expr = e} = check e
  check RecordUpd { rupd_expr = e, rupd_flds = fs} = (&&) <$> check e <*> check fs
  check RecordCon { rcon_flds = f} = check f
  check (ArithSeq _ _ e) = check e
#if __GLASGOW_HASKELL__ >= 906
  check HsTypedSplice{} = notSupported "Template Haskell"
  check HsUntypedSplice{} = notSupported "Template Haskell"
#else
  check HsSpliceE{} = notSupported "Template Haskell"
#endif
  check (HsProc _ _ e) = check e
  check (HsStatic _ e) = check e
  check (HsDo _ _ e) = fst <$> checkBind e
  check (XExpr e) = check e
#if __GLASGOW_HASKELL__ >= 906
  check (HsAppType _ e _ _) = check e
  check (ExprWithTySig _ e _) = check e
#else
  check (HsAppType _ e _) = check e
  check (ExprWithTySig _ e _) = check e
#endif
  check (HsPragE _ _ e) = check e
  check (HsIf _ e1 e2 e3) = and <$> mapM check [e1,e2,e3]


instance (Scope a, Scope b) => Scope (Either a b) where
  check (Left x) = check x
  check (Right x) = check x


#if __GLASGOW_HASKELL__ >= 908
instance Scope (LHsRecUpdFields GhcTc) where
  check RegularRecUpdFields {recUpdFields = x} = check x
  check OverloadedRecUpdFields {olRecUpdFields = x} = check x
#endif


instance Scope XXExprGhcTc where
  check (WrapExpr (HsWrap _ e)) = check e
  check (ExpansionExpr (HsExpanded _ e)) = check e
#if __GLASGOW_HASKELL__ >= 904
  check ConLikeTc{} = return True
  check (HsTick _ e) = check e
  check (HsBinTick _ _ e) = check e
#endif

instance Scope (HsCmdTop GhcTc) where
  check (HsCmdTop _ e) = check e
  
instance Scope (HsCmd GhcTc) where
  check (HsCmdArrApp _ e1 e2 _ _) = (&&) <$> check e1 <*> check e2
  check (HsCmdDo _ e) = fst <$> checkBind e
  check (HsCmdArrForm _ e1 _ _ e2) = (&&) <$> check e1 <*> check e2
  check (HsCmdApp _ e1 e2) = (&&) <$> check e1 <*> check e2
  check (HsCmdLam _ e) = check e
#if __GLASGOW_HASKELL__ >= 904
  check (HsCmdPar _ _ e _) = check e
  check (HsCmdLamCase _ _ e) = check e  
  check (HsCmdLet _ _ bs _ e) = do
#else
  check (HsCmdPar _ e) = check e
  check (HsCmdLamCase _ e) = check e
  check (HsCmdLet _ bs e) = do
#endif
    (l,vs) <- checkBind bs
    r <- addVars vs `modifyCtxt` (check e)
    return (r && l)

  check (HsCmdCase _ e1 e2) = (&&) <$> check e1 <*> check e2
  check (HsCmdIf _ _ e1 e2 e3) = (&&) <$> ((&&) <$> check e1 <*> check e2) <*> check e3
  check (XCmd (HsWrap _ e)) = check e


instance Scope (ArithSeqInfo GhcTc) where
  check (From e) = check e
  check (FromThen e1 e2) = (&&) <$> check e1 <*> check e2
  check (FromTo e1 e2) = (&&) <$> check e1 <*> check e2
  check (FromThenTo e1 e2 e3) = (&&) <$> ((&&) <$> check e1 <*> check e2) <*> check e3

instance Scope a => Scope (HsRecFields GhcTc a) where
  check HsRecFields {rec_flds = fs} = check fs



#if __GLASGOW_HASKELL__ >= 904
instance Scope b => Scope (HsFieldBind a b) where
  check HsFieldBind{hfbRHS = a} = check a
#else
instance Scope b => Scope (HsRecField' a b) where
  check HsRecField{hsRecFieldArg = a} = check a
#endif

instance Scope (HsTupArg GhcTc) where
  check (Present _ e) = check e
  check Missing{} = return True

instance Scope (HsBindLR GhcTc GhcTc) where
#if __GLASGOW_HASKELL__ >= 904
  check (XHsBindsLR AbsBinds {abs_binds = binds, abs_ev_vars  = ev})
#else
  check AbsBinds {abs_binds = binds, abs_ev_vars  = ev}
#endif
    = mod `modifyCtxt` check binds
      where mod c = c { stableTypes= stableTypes c `Set.union`
                        Set.fromList (mapMaybe (isStableConstr . varType) ev)}
  check FunBind{fun_matches= matches, fun_id = L _ v,
                fun_ext = wrapper} =
      mod `modifyCtxt` check matches
    where mod c = c { stableTypes= stableTypes c `Set.union`
                      Set.fromList (stableConstrFromWrapper' wrapper)  `Set.union`
                      Set.fromList (extractStableConstr (varType v))}
  check PatBind{pat_lhs = lhs, pat_rhs=rhs} = addVars (getBV lhs) `modifyCtxt` check rhs
  check VarBind{var_rhs = rhs} = check rhs
  check PatSynBind {} = return True -- pattern synonyms are not supported


-- | Checks whether the given type is a type constraint of the form
-- @Stable a@ for some type variable @a@. In that case it returns the
-- type variable @a@.
isStableConstr :: Type -> Maybe TyVar
isStableConstr t = 
  case splitTyConApp_maybe t of
    Just (con,[args]) ->
      case getNameModule con of
        Just (name, mod) ->
          if isRattModule mod && name == "Stable"
          then (getTyVar_maybe args)
          else Nothing
        _ -> Nothing                           
    _ ->  Nothing



#if __GLASGOW_HASKELL__ >= 906
stableConstrFromWrapper' :: (HsWrapper , a) -> [TyVar]
stableConstrFromWrapper' (x , _) = stableConstrFromWrapper x
#else
stableConstrFromWrapper' :: HsWrapper -> [TyVar]
stableConstrFromWrapper' = stableConstrFromWrapper
#endif

stableConstrFromWrapper :: HsWrapper -> [TyVar]
stableConstrFromWrapper (WpCompose v w) = stableConstrFromWrapper v ++ stableConstrFromWrapper w
stableConstrFromWrapper (WpEvLam v) = maybeToList $ isStableConstr (varType v)
stableConstrFromWrapper _ = []


-- | Given a type @(C1, ... Cn) => t@, this function returns the list
-- of type variables @[a1,...,am]@ for which there is a constraint
-- @Stable ai@ among @C1, ... Cn@.
extractStableConstr :: Type -> [TyVar]
extractStableConstr  = mapMaybe isStableConstr . map irrelevantMult . fst . splitFunTys . snd . splitForAllTys'


getSCCLoc :: SCC (LHsBindLR  GhcTc GhcTc, Set Var) -> SrcSpan
getSCCLoc (AcyclicSCC (L l _ ,_)) = getLocAnn' l
getSCCLoc (CyclicSCC ((L l _,_ ) : _)) = getLocAnn' l
getSCCLoc _ = noLocationInfo

checkSCC' ::  Module -> AnnEnv -> SCC (LHsBindLR  GhcTc GhcTc, Set Var) -> TcM (Bool, [ErrorMsg])
checkSCC' mod anEnv scc = do
  err <- liftIO (newIORef [])
  let allowRec = AllowRecursion `Set.member` getAnn mod anEnv scc
  res <- checkSCC allowRec err scc
  msgs <- liftIO (readIORef err)
  let anns = getAnn mod anEnv scc
  if ExpectWarning `Set.member` anns 
    then if ExpectError `Set.member` anns
         then return (False,[(SevError, getSCCLoc scc, "Annotation to expect both warning and error is not allowed.")])
         else if any (\(s,_,_) -> case s of SevWarning -> True; _ -> False) msgs
              then return (res, filter (\(s,_,_) -> case s of SevWarning -> False; _ -> True) msgs)
              else return (False,[(SevError, getSCCLoc scc, "Warning was expected, but typechecking produced no warning.")])
    else if ExpectError `Set.member` anns
         then if res
              then return (False,[(SevError, getSCCLoc scc, "Error was expected, but typechecking produced no error.")])
              else return (True,[])
         else return (res, msgs)
getAnn :: forall a . (Data a, Ord a) => Module -> AnnEnv -> SCC (LHsBindLR  GhcTc GhcTc, Set Var) -> Set a
getAnn mod anEnv scc =
  case scc of
    (AcyclicSCC (_,vs)) -> Set.unions $ Set.map checkVar vs
    (CyclicSCC bs) -> Set.unions $ map (Set.unions . Set.map checkVar . snd) bs
  where checkVar :: Var -> Set a
        checkVar v =
          let anns = findAnns deserializeWithData anEnv (NamedTarget name) :: [a]
              annsMod = findAnns deserializeWithData anEnv (ModuleTarget mod) :: [a]
              name :: Name
              name = varName v
          in Set.fromList anns `Set.union` Set.fromList annsMod



-- | Checks a top-level definition group, which is either a single
-- non-recursive definition or a group of (mutual) recursive
-- definitions.

checkSCC :: Bool -> ErrorMsgsRef -> SCC (LHsBindLR  GhcTc GhcTc, Set Var) -> TcM Bool
checkSCC allowRec errm (AcyclicSCC (b,_)) = setCtxt (emptyCtxt errm Nothing allowRec) (evalStateT (check b) [])

checkSCC allowRec errm (CyclicSCC bs) = (fmap and (mapM check' bs'))
  where bs' = map fst bs
        vs = foldMap snd bs
        check' b@(L l _) = setCtxt (emptyCtxt errm (Just (vs,getLocAnn' l)) allowRec) (evalStateT (checkRec b) [])

-- | Stabilizes the given context, i.e. remove all non-stable types
-- and any tick. This is performed on checking 'box', and
-- guarded recursive definitions. To provide better error messages a
-- reason has to be given as well.
stabilize :: StableReason -> Ctxt -> Ctxt
stabilize sr c = c
  {current = Set.empty,
   earlier = Left $ TickHidden hr,
   hidden = hidden c `Map.union` Map.fromSet (const hr) ctxHid}
  where ctxHid = either (const $ current c) (foldl' Set.union (current c)) (earlier c)
        hr = Stabilize sr

data VarScope = Hidden SDoc | Visible | ImplUnboxed


-- | This function checks whether the given variable is in scope.
getScope  :: GetCtxt => Var -> VarScope
getScope v =
  case ?ctxt of
    Ctxt{recDef = Just (vs,_), earlier = e, allowRecursion = allowRec} | v `Set.member` vs ->
     if allowRec then Visible else
        case e of
          Right _ -> Visible
          Left NoDelay -> Hidden ("The (mutually) recursive call to " <> ppr v <> " must occur in the scope of a delay")
          Left (TickHidden hr) -> Hidden ("The (mutually) recursive call to " <> ppr v <> " must occur in the scope of a delay. "
                            <> "There is a delay, but its scope is interrupted by " <> tickHidden hr <> ".")
    _ ->  case Map.lookup v (hidden ?ctxt) of
            Just (Stabilize (StableRec rv)) ->
              if (isStable (stableTypes ?ctxt) (varType v)) || allowRecursion ?ctxt then Visible
              else Hidden ("Variable " <> ppr v <> " is no longer in scope:" $$
                       "It appears in a local recursive definition (at " <> ppr rv <> ")"
                       $$ "and is of type " <> ppr (varType v) <> ", which is not stable.")
            Just (Stabilize StableBox) ->
              if (isStable (stableTypes ?ctxt) (varType v)) then Visible
              else Hidden ("Variable " <> ppr v <> " is no longer in scope:" $$
                       "It occurs under " <> keyword "box" $$ "and is of type " <> ppr (varType v) <> ", which is not stable.")
            Just AdvApp -> Hidden ("Variable " <> ppr v <> " is no longer in scope: It occurs under adv.")
            Just SelectApp -> Hidden ("Variable " <> ppr v <> " is no longer in scope: It occurs under select.")
            Just DelayApp -> Hidden ("Variable " <> ppr v <> " is no longer in scope due to repeated application of delay")
            Just FunDef -> if (isStable (stableTypes ?ctxt) (varType v)) then Visible
              else Hidden ("Variable " <> ppr v <> " is no longer in scope: It occurs in a function that is defined under a delay, is a of a non-stable type " <> ppr (varType v) <> ", and is bound outside delay")
            Nothing
              | either (const False) (any (Set.member v)) (earlier ?ctxt) ->
                if isStable (stableTypes ?ctxt) (varType v) then Visible
                else Hidden ("Variable " <> ppr v <> " is no longer in scope:" $$
                         "It occurs under delay" $$ "and is of type " <> ppr (varType v) <> ", which is not stable.")
              | Set.member v (current ?ctxt) -> Visible
              | isTemporal (varType v) && isRight (earlier ?ctxt) && userFunction v
                -> ImplUnboxed
              | otherwise -> Visible

-- | A map from the syntax of a primitive of Asynchronous Rattus to 'Prim'.
primMap :: Map FastString Prim
primMap = Map.fromList
  [("Delay", Delay),
   ("delay", Delay),
   ("adv", Adv),
   ("select", Select),
   ("box", Box),
   ("unbox", Unbox)]


-- | Checks whether a given variable is in fact an Asynchronous Rattus primitive.
isPrim :: GetCtxt => Var -> Maybe Prim
isPrim v
  | Just p <- Map.lookup v (primAlias ?ctxt) = Just p
  | otherwise = do
  (name,mod) <- getNameModule v
  if isRattModule mod then Map.lookup name primMap
  else Nothing


-- | Checks whether a given expression is in fact a Asynchronous Rattus primitive.
isPrimExpr :: GetCtxt => LHsExpr GhcTc -> Maybe (Prim,Var)
isPrimExpr (L _ e) = isPrimExpr' e where
  isPrimExpr' :: GetCtxt => HsExpr GhcTc -> Maybe (Prim,Var)
  isPrimExpr' (HsVar _ (L _ v)) = fmap (,v) (isPrim v)
#if __GLASGOW_HASKELL__ >= 906
  isPrimExpr' (HsAppType _ e _ _) = isPrimExpr e
#else
  isPrimExpr' (HsAppType _ e _) = isPrimExpr e
#endif

  isPrimExpr' (XExpr (WrapExpr (HsWrap _ e))) = isPrimExpr' e
  isPrimExpr' (XExpr (ExpansionExpr (HsExpanded _ e))) = isPrimExpr' e
  isPrimExpr' (HsPragE _ _ e) = isPrimExpr e
#if __GLASGOW_HASKELL__ < 904
  isPrimExpr' (HsTick _ _ e) = isPrimExpr e
  isPrimExpr' (HsBinTick _ _ _ e) = isPrimExpr e
  isPrimExpr' (HsPar _ e) = isPrimExpr e
#else
  isPrimExpr' (XExpr (HsTick _ e)) = isPrimExpr e
  isPrimExpr' (XExpr (HsBinTick _ _ e)) = isPrimExpr e
  isPrimExpr' (HsPar _ _ e _) = isPrimExpr e
#endif

  isPrimExpr' _ = Nothing


-- | This type class provides default implementations for 'check' and
-- 'checkBind' for Haskell syntax that is not supported. These default
-- implementations simply print an error message.
class NotSupported a where
  notSupported :: GetCtxt => SDoc -> CheckM a

instance NotSupported Bool where
  notSupported doc = printMessageCheck SevError ("Asynchronous Rattus does not support " <> doc)

instance NotSupported (Bool,Set Var) where
  notSupported doc = (,Set.empty) <$> notSupported doc


-- | Add variables to the current context.
addVars :: Set Var -> Ctxt -> Ctxt
addVars vs c = c{current = vs `Set.union` current c }

-- | Print a message with the current location.
printMessage' :: GetCtxt => Severity -> SDoc ->  CheckM ()
printMessage' sev doc =
  liftIO (modifyIORef (errorMsgs ?ctxt) ((sev ,srcLoc ?ctxt, doc) :))

-- | Print a message with the current location. Returns 'False', if
-- the severity is 'SevError' and otherwise 'True.
printMessageCheck :: GetCtxt =>  Severity -> SDoc -> CheckM Bool
printMessageCheck sev doc = printMessage' sev doc >>
  case sev of
    SevError -> return False
    _ -> return True
