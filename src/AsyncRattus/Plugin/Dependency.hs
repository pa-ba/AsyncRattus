{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}

-- | This module is used to perform a dependency analysis of top-level
-- function definitions, i.e. to find out which defintions are
-- (mutual) recursive. To this end, this module also provides
-- functions to compute, bound variables and variable occurrences.

module Rattus.Plugin.Dependency (dependency, HasBV (..),printBinds) where

#if __GLASGOW_HASKELL__ >= 900
import GHC.Plugins
import GHC.Data.Bag
import GHC.Hs.Type
#else
import GhcPlugins
import Bag
#if __GLASGOW_HASKELL__ >= 810
import GHC.Hs.Types
#else
import HsTypes
#endif
#endif

#if __GLASGOW_HASKELL__ >= 810
import GHC.Hs.Extension
import GHC.Hs.Expr
import GHC.Hs.Pat
import GHC.Hs.Binds
#else 
import HsExtension
import HsExpr
import HsPat
import HsBinds
#endif

#if __GLASGOW_HASKELL__ >= 904
import GHC.Parser.Annotation
#elif __GLASGOW_HASKELL__ >= 902
import Language.Haskell.Syntax.Extension
import GHC.Parser.Annotation
#endif


import Data.Set (Set)
import qualified Data.Set as Set
import Data.Graph
import Data.Maybe
import Data.Either
import Prelude hiding ((<>))



-- | Compute the dependencies of a bag of bindings, returning a list
-- of the strongly-connected components.
dependency :: Bag (LHsBindLR GhcTc GhcTc) -> [SCC (LHsBindLR GhcTc GhcTc, Set Var)]
dependency binds = map AcyclicSCC noDeps ++ catMaybes (map filterJust (stronglyConnComp (concat deps)))
  where (deps,noDeps) = partitionEithers $ map mkDep $ bagToList binds
        mkDep :: GenLocated l (HsBindLR GhcTc GhcTc) ->
                 Either [(Maybe (GenLocated l (HsBindLR GhcTc GhcTc), Set Var), Name, [Name])]
                 (GenLocated l (HsBindLR GhcTc GhcTc), Set Var)
        mkDep b =
          let dep = map varName $ Set.toList (getFV b)
              vars = getBV b in
          case Set.toList vars of
            (v:vs) -> Left ((Just (b,vars), varName v , dep) : map (\ v' -> (Nothing, varName v' , dep)) vs)
            [] -> Right (b,vars)
        filterJust (AcyclicSCC Nothing) = Nothing -- this should not happen
        filterJust (AcyclicSCC (Just b)) = Just (AcyclicSCC b)
        filterJust (CyclicSCC bs) = Just (CyclicSCC (catMaybes bs))


printBinds (AcyclicSCC bind) = liftIO (putStr "acyclic bind: ") >> printBind (fst bind) >> liftIO (putStrLn "") 
printBinds (CyclicSCC binds) = liftIO (putStr "cyclic binds: ") >> mapM_ (printBind . fst) binds >> liftIO (putStrLn "") 


printBind (L _ FunBind{fun_id = L _ name}) = 
  liftIO $ putStr $ (getOccString name ++ " ")
printBind (L _ (VarBind {var_id = name})) =   liftIO $ putStr $ (getOccString name ++ " ")
#if __GLASGOW_HASKELL__ < 904
printBind (L _ (AbsBinds {abs_exports = exp})) = 
#else
printBind (L _ (XHsBindsLR (AbsBinds {abs_exports = exp}))) = 
#endif
  mapM_ (\ e -> liftIO $ putStr $ ((getOccString $ abe_poly e)  ++ " ")) exp
printBind _ = return ()


-- | Computes the variables that are bound by a given piece of syntax.

class HasBV a where
  getBV :: a -> Set Var

instance HasBV (HsBindLR GhcTc GhcTc) where
  getBV (FunBind{fun_id = L _ v}) = Set.singleton v
  getBV (PatBind {pat_lhs = pat}) = getBV pat
  getBV (VarBind {var_id = v}) = Set.singleton v
  getBV PatSynBind{} = Set.empty
#if __GLASGOW_HASKELL__ < 900
  getBV (XHsBindsLR e) = getBV e
  getBV (AbsBinds {abs_exports = es}) = Set.fromList (map abe_poly es)
#elif __GLASGOW_HASKELL__ < 904
  getBV (AbsBinds {abs_exports = es}) = Set.fromList (map abe_poly es)
#else
  getBV (XHsBindsLR (AbsBinds {abs_exports = es})) = Set.fromList (map abe_poly es)
#endif
  
instance HasBV a => HasBV (GenLocated b a) where
  getBV (L _ e) = getBV e

instance HasBV a => HasBV [a] where
  getBV ps = foldl (\s p -> getBV p `Set.union` s) Set.empty ps

#if __GLASGOW_HASKELL__ >= 904
getRecFieldRhs = hfbRHS
#else
getRecFieldRhs = hsRecFieldArg
#endif

#if __GLASGOW_HASKELL__ >= 902
getConBV (PrefixCon _ ps) = getBV ps
#else
getConBV (PrefixCon ps) = getBV ps
#endif
getConBV (InfixCon p p') = getBV p `Set.union` getBV p'
getConBV (RecCon (HsRecFields {rec_flds = fs})) = foldl run Set.empty fs
      where run s (L _ f) = getBV (getRecFieldRhs f) `Set.union` s

#if __GLASGOW_HASKELL__ >= 900 && __GLASGOW_HASKELL__ < 904
instance HasBV CoPat where
  getBV CoPat {co_pat_inner = p} = getBV p
#elif __GLASGOW_HASKELL__ >= 904
instance HasBV XXPatGhcTc where
  getBV CoPat {co_pat_inner = p} = getBV p
  getBV (ExpansionPat _ p) = getBV p
#endif

instance HasBV (Pat GhcTc) where
  getBV (VarPat _ (L _ v)) = Set.singleton v
  getBV (LazyPat _ p) = getBV p
#if __GLASGOW_HASKELL__ >= 906
  getBV (AsPat _ (L _ v) _ p) = Set.insert v (getBV p)
#else
  getBV (AsPat _ (L _ v) p) = Set.insert v (getBV p)
#endif
  getBV (BangPat _ p) = getBV p
  getBV (ListPat _ ps) = getBV ps
  getBV (TuplePat _ ps _) = getBV ps
  getBV (SumPat _ p _ _) = getBV p
  getBV (ViewPat _ _ p) = getBV p

  getBV (SplicePat _ sp) =
    case sp of
#if __GLASGOW_HASKELL__ < 906
      HsTypedSplice _ _ v _ -> Set.singleton v
      HsSpliced _ _ (HsSplicedPat p) -> getBV p
      HsUntypedSplice _ _ v _ ->  Set.singleton v
      HsQuasiQuote _ p p' _ _ -> Set.fromList [p,p']
      _ -> Set.empty
#else
      HsUntypedSpliceExpr _ e -> getFV e
      HsQuasiQuote _ v _  -> Set.singleton v
#endif

  getBV (NPlusKPat _ (L _ v) _ _ _ _) = Set.singleton v
  getBV (NPat {}) = Set.empty
  getBV (XPat p) = getBV p
  getBV (WildPat {}) = Set.empty
  getBV (LitPat {}) = Set.empty
#if __GLASGOW_HASKELL__ >= 904  
  getBV (ParPat _ _ p _) = getBV p
#else
  getBV (ParPat _ p) = getBV p
#endif
#if __GLASGOW_HASKELL__ >= 900
  getBV (ConPat {pat_args = con}) = getConBV con
#else
  getBV (ConPatIn (L _ v) con) = Set.insert v (getConBV con)
  getBV (ConPatOut {pat_args = con}) = getConBV con
  getBV (CoPat _ _ p _) = getBV p
#endif
#if __GLASGOW_HASKELL__ >= 808
  getBV (SigPat _ p _) = getBV p
#else
  getBV (SigPat _ p)   = getBV p
#endif

#if __GLASGOW_HASKELL__ >= 904

#elif __GLASGOW_HASKELL__ >= 810
instance HasBV NoExtCon where
#else
instance HasBV NoExt where
#endif
#if __GLASGOW_HASKELL__ < 904
  getBV _ = Set.empty
#endif

-- | Syntax that may contain variables.
class HasFV a where
  -- | Compute the set of variables occurring in the given piece of
  -- syntax.  The name falsely suggests that returns free variables,
  -- but in fact it returns all variable occurrences, no matter
  -- whether they are free or bound.
  getFV :: a -> Set Var 

instance HasFV a => HasFV (GenLocated b a) where
  getFV (L _ e) = getFV e
  
instance HasFV a => HasFV [a] where
  getFV es = foldMap getFV es

instance HasFV a => HasFV (Bag a) where
  getFV es = foldMap getFV es

instance HasFV Var where
  getFV v = Set.singleton v

instance HasFV a => HasFV (MatchGroup GhcTc a) where
  getFV MG {mg_alts = alts} = getFV alts
#if __GLASGOW_HASKELL__ < 900
  getFV (XMatchGroup e) = getFV e
#endif
  
instance HasFV a => HasFV (Match GhcTc a) where
  getFV Match {m_grhss = rhss} = getFV rhss
#if __GLASGOW_HASKELL__ < 900
  getFV (XMatch e) = getFV e
#endif

instance HasFV (HsTupArg GhcTc) where
  getFV (Present _ e) = getFV e
  getFV Missing {} = Set.empty
#if __GLASGOW_HASKELL__ < 900
  getFV (XTupArg e) = getFV e
#endif

instance HasFV a => HasFV (GRHS GhcTc a) where
  getFV (GRHS _ g b) = getFV g `Set.union` getFV b
#if __GLASGOW_HASKELL__ < 900
  getFV (XGRHS e) = getFV e
#endif

instance HasFV a => HasFV (GRHSs GhcTc a) where
  getFV GRHSs {grhssGRHSs = rhs, grhssLocalBinds = lbs} =
    getFV rhs `Set.union` getFV lbs
#if __GLASGOW_HASKELL__ < 900
  getFV (XGRHSs e) = getFV e
#endif


instance HasFV (HsLocalBindsLR GhcTc GhcTc) where
  getFV (HsValBinds _ bs) = getFV bs
  getFV (HsIPBinds _ bs) = getFV bs
  getFV EmptyLocalBinds {} = Set.empty
#if __GLASGOW_HASKELL__ < 900
  getFV (XHsLocalBindsLR e) = getFV e
#endif
  
instance HasFV (HsValBindsLR GhcTc GhcTc) where
  getFV (ValBinds _ b _) = getFV b
  getFV (XValBindsLR b) = getFV b

instance HasFV (NHsValBindsLR GhcTc) where
  getFV (NValBinds bs _) = foldMap (getFV . snd) bs

instance HasFV (HsBindLR GhcTc GhcTc) where
  getFV FunBind {fun_matches = ms} = getFV ms
  getFV PatBind {pat_rhs = rhs} = getFV rhs
  getFV VarBind {var_rhs = rhs} = getFV rhs
  getFV PatSynBind {} = Set.empty
#if __GLASGOW_HASKELL__ < 904
  getFV AbsBinds {abs_binds = bs} = getFV bs
#else
  getFV (XHsBindsLR AbsBinds {abs_binds = bs}) = getFV bs
#endif
#if __GLASGOW_HASKELL__ < 900
  getFV (XHsBindsLR e) = getFV e
#endif

instance HasFV (IPBind GhcTc) where
  getFV (IPBind _ _ e) = getFV e
#if __GLASGOW_HASKELL__ < 900
  getFV (XIPBind e) = getFV e
#endif

instance HasFV (HsIPBinds GhcTc) where
  getFV (IPBinds _ bs) = getFV bs
#if __GLASGOW_HASKELL__ < 900
  getFV (XHsIPBinds e) = getFV e
#endif
  
instance HasFV (ApplicativeArg GhcTc) where
#if __GLASGOW_HASKELL__ >= 810
  getFV ApplicativeArgOne { arg_expr = e }     = getFV e
  getFV ApplicativeArgMany {app_stmts = es, final_expr = e} = getFV es `Set.union` getFV e
#else
  getFV (ApplicativeArgOne _ _ e _) = getFV e
  getFV (ApplicativeArgMany _ es e _) = getFV es `Set.union` getFV e
#endif
#if __GLASGOW_HASKELL__ < 900
  getFV (XApplicativeArg e) = getFV e
#endif

instance HasFV (ParStmtBlock GhcTc GhcTc) where
  getFV (ParStmtBlock _ es _ _) = getFV es
#if __GLASGOW_HASKELL__ < 900
  getFV (XParStmtBlock e) = getFV e
#endif
  
instance HasFV a => HasFV (StmtLR GhcTc GhcTc a) where
  getFV (LastStmt _ e _ _) = getFV e
#if __GLASGOW_HASKELL__ >= 900
  getFV (BindStmt _ _ e) = getFV e
#else
  getFV (BindStmt _ _ e _ _) = getFV e
#endif
  getFV (ApplicativeStmt _ args _) = foldMap (getFV . snd) args
  getFV (BodyStmt _ e _ _) = getFV e
  getFV (LetStmt _ bs) = getFV bs
  getFV (ParStmt _ stms e _) = getFV stms `Set.union` getFV e
  getFV TransStmt{} = Set.empty -- TODO
  getFV RecStmt{} = Set.empty -- TODO
#if __GLASGOW_HASKELL__ < 900
  getFV (XStmtLR e) = getFV e
#endif

#if __GLASGOW_HASKELL__ >= 902
instance HasFV (HsRecFields GhcTc (GenLocated SrcSpanAnnA (HsExpr GhcTc))) where
#else
instance HasFV (HsRecordBinds GhcTc) where
#endif
  getFV HsRecFields{rec_flds = fs} = getFV fs

#if __GLASGOW_HASKELL__ >= 904
instance HasFV (HsFieldBind o (GenLocated SrcSpanAnnA (HsExpr GhcTc))) where
#elif __GLASGOW_HASKELL__ >= 902
instance HasFV (HsRecField' o (GenLocated SrcSpanAnnA (HsExpr GhcTc))) where
#else
instance HasFV (HsRecField' o (LHsExpr GhcTc)) where
#endif
  getFV rf  = getFV (getRecFieldRhs rf)

instance HasFV (ArithSeqInfo GhcTc) where
  getFV (From e) = getFV e
  getFV (FromThen e1 e2) = getFV e1 `Set.union` getFV e2
  getFV (FromTo e1 e2) = getFV e1 `Set.union` getFV e2
  getFV (FromThenTo e1 e2 e3) = getFV e1 `Set.union` getFV e2 `Set.union` getFV e3
  
#if __GLASGOW_HASKELL__ >= 904
instance HasFV (HsQuote GhcTc) where
#else
instance HasFV (HsBracket GhcTc) where
#endif
  getFV (ExpBr _ e) = getFV e
  getFV (VarBr _ _ e) = getFV e
  getFV _ = Set.empty

instance HasFV (HsCmd GhcTc) where
  getFV (HsCmdArrApp _ e1 e2 _ _) = getFV e1 `Set.union` getFV e2
  getFV (HsCmdArrForm _ e _ _ cmd) = getFV e `Set.union` getFV cmd
  getFV (HsCmdApp _ e1 e2) = getFV e1 `Set.union` getFV e2
  getFV (HsCmdLam _ l) = getFV l
  getFV (HsCmdCase _ _ mg) = getFV mg
  getFV (HsCmdIf _ _ e1 e2 e3) = getFV e1 `Set.union` getFV e2 `Set.union` getFV e3
  getFV (HsCmdDo _ cmd) = getFV cmd
#if __GLASGOW_HASKELL__ >= 904
  getFV (HsCmdPar _ _ cmd _) = getFV cmd
  getFV (HsCmdLet _ _ bs _ _) = getFV bs
#else
  getFV (HsCmdPar _ cmd) = getFV cmd
  getFV (HsCmdLet _ bs _) = getFV bs
#endif
#if __GLASGOW_HASKELL__ >= 904
  getFV (HsCmdLamCase _ _ mg) = getFV mg
#elif __GLASGOW_HASKELL__ >= 900
  getFV (HsCmdLamCase _ mg) = getFV mg
#else
  getFV (HsCmdWrap _ _ cmd) = getFV cmd
#endif
  getFV (XCmd e) = getFV e
  

instance HasFV (HsCmdTop GhcTc) where
  getFV (HsCmdTop _ cmd) = getFV cmd
#if __GLASGOW_HASKELL__ < 900
  getFV (XCmdTop e) = getFV e
#endif

instance HasFV (HsExpr GhcTc) where
  getFV (HsVar _ v) = getFV v
  getFV HsUnboundVar {} = Set.empty
  getFV HsOverLabel {} = Set.empty
  getFV HsIPVar {} = Set.empty
  getFV HsOverLit {} = Set.empty
  getFV HsLit {} = Set.empty
  getFV (HsLam _ mg) = getFV mg
  getFV (HsApp _ e1 e2) = getFV e1 `Set.union` getFV e2      
  getFV (OpApp _ e1 e2 e3) = getFV e1 `Set.union` getFV e2 `Set.union` getFV e3
  getFV (NegApp _ e _) = getFV e
  getFV (SectionL _ e1 e2) = getFV e1 `Set.union` getFV e2
  getFV (SectionR _ e1 e2) = getFV e1 `Set.union` getFV e2
  getFV (ExplicitTuple _ es _) = getFV es
  getFV (ExplicitSum _ _ _ e) = getFV e
  getFV (HsCase _ e mg) = getFV e  `Set.union` getFV mg
  getFV (HsMultiIf _ es) = getFV es
  getFV (HsDo _ _ e) = getFV e
#if __GLASGOW_HASKELL__ >= 902
  getFV HsProjection {} = Set.empty
  getFV HsGetField {gf_expr = e} = getFV e
  getFV (ExplicitList _ es) = getFV es
  getFV (RecordUpd {rupd_expr = e, rupd_flds = fs}) =
    getFV e `Set.union` either getFV getFV fs
#else
  getFV (ExplicitList _ _ es) = getFV es
  getFV (RecordUpd {rupd_expr = e, rupd_flds = fs}) = getFV e `Set.union` getFV fs
#endif
  getFV (RecordCon {rcon_flds = fs}) = getFV fs
  getFV (ArithSeq _ _ e) = getFV e
#if __GLASGOW_HASKELL__ >= 906
  getFV HsTypedSplice{} = Set.empty
  getFV HsUntypedSplice{} = Set.empty
#else
  getFV HsSpliceE{} = Set.empty
#endif
  getFV (HsProc _ _ e) = getFV e
  getFV (HsStatic _ e) = getFV e
  getFV (XExpr e) = getFV e
#if __GLASGOW_HASKELL__ >= 904
  getFV (HsPar _ _ e _) = getFV e  
  getFV (HsLamCase _ _ mg) = getFV mg
  getFV (HsLet _ _ bs _ e) = getFV bs `Set.union` getFV e
  getFV HsRecSel {} = Set.empty
  getFV (HsTypedBracket _ e) = getFV e
  getFV (HsUntypedBracket _ e) = getFV e
#else  
  getFV (HsBinTick _ _ _ e) = getFV e
  getFV (HsTick _ _ e) = getFV e
  getFV (HsLet _ bs e) = getFV bs `Set.union` getFV e
  getFV (HsPar _ e) = getFV e
  getFV (HsLamCase _ mg) = getFV mg
  getFV HsConLikeOut {} = Set.empty
  getFV HsRecFld {} = Set.empty
  getFV (HsBracket _ e) = getFV e
  getFV HsRnBracketOut {} = Set.empty
  getFV HsTcBracketOut {} = Set.empty
#endif

#if __GLASGOW_HASKELL__ >= 906
  getFV (HsAppType _ e _ _) = getFV e
  getFV (ExprWithTySig _ e _) = getFV e  
#elif __GLASGOW_HASKELL__ >= 808
  getFV (HsAppType _ e _) = getFV e
  getFV (ExprWithTySig _ e _) = getFV e  
#else
  getFV (ExprWithTySig _ e)   = getFV e
  getFV (HsAppType _ e)   = getFV e
#endif

#if __GLASGOW_HASKELL__ >= 900
  getFV (HsIf _ e1 e2 e3) = getFV e1 `Set.union` getFV e2 `Set.union` getFV e3
  getFV (HsPragE _ _ e) = getFV e
#else
  getFV (HsIf _ _ e1 e2 e3) = getFV e1 `Set.union` getFV e2 `Set.union` getFV e3
  getFV (HsSCC _ _ _ e) = getFV e
  getFV (HsCoreAnn _ _ _ e) = getFV e
  getFV (HsTickPragma _ _ _ _ e) = getFV e
  getFV (HsWrap _ _ e) = getFV e
#endif

#if __GLASGOW_HASKELL__ < 810  
  getFV (HsArrApp _ e1 e2 _ _) = getFV e1 `Set.union` getFV e2
  getFV (HsArrForm _ e _ cmd) = getFV e `Set.union` getFV cmd
  getFV EWildPat {} = Set.empty
  getFV (EAsPat _ e1 e2) = getFV e1 `Set.union` getFV e2
  getFV (EViewPat _ e1 e2) = getFV e1 `Set.union` getFV e2
  getFV (ELazyPat _ e) = getFV e
#endif


#if __GLASGOW_HASKELL__ >= 900
instance HasFV XXExprGhcTc where
  getFV (WrapExpr e) = getFV e
  getFV (ExpansionExpr (HsExpanded _e1 e2)) = getFV e2
#if __GLASGOW_HASKELL__ >= 904  
  getFV (HsTick _ e) = getFV e
  getFV (HsBinTick _ _ e) = getFV e
  getFV ConLikeTc{} = Set.empty
#endif


instance HasFV (e GhcTc) => HasFV (HsWrap e) where
  getFV (HsWrap _ e) = getFV e
#elif __GLASGOW_HASKELL__ >= 810
instance HasFV NoExtCon where
  getFV _ = Set.empty
#else
instance HasFV NoExt where
  getFV _ = Set.empty
#endif
