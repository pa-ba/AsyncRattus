{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Rattus.Plugin.Strictify
  (strictifyExpr, SCxt (..)) where
import Prelude hiding ((<>))
import Rattus.Plugin.Utils

#if __GLASGOW_HASKELL__ >= 900
import GHC.Plugins
#else
import GhcPlugins
#endif

#if __GLASGOW_HASKELL__ >= 902
import GHC.Types.Tickish
#endif

data SCxt = SCxt {srcSpan :: SrcSpan, checkStrictData :: Bool}

-- | Transforms all functions into strict functions. If the
-- 'checkStrictData' field of the 'SCxt' argument is set to @True@,
-- then this function also checks for use of non-strict data types and
-- produces warnings if it finds any.
strictifyExpr :: SCxt -> CoreExpr -> CoreM CoreExpr
strictifyExpr ss (Let (NonRec b e1) e2) = do
  e1' <- strictifyExpr ss e1
  e2' <- strictifyExpr ss e2
  return (Case e1' b (exprType e2) [mkAlt DEFAULT [] e2' ])
strictifyExpr ss (Case e b t alts) = do
  e' <- strictifyExpr ss e
  alts' <- mapM ((\(c,args,e) -> fmap (\e' -> mkAlt c args e' ) (strictifyExpr ss e)) . getAlt) alts
  return (Case e' b t alts')
strictifyExpr ss (Let (Rec es) e) = do
  es' <- mapM (\ (b,e) -> strictifyExpr ss e >>= \e'-> return (b,e')) es
  e' <- strictifyExpr ss e
  return (Let (Rec es') e')
strictifyExpr ss (Lam b e)
   | not (isCoVar b) && not (isTyVar b) && tcIsLiftedTypeKind(typeKind (varType b))
    = do
       e' <- strictifyExpr ss e
       b' <- mkSysLocalFromVar (fsLit "strict") b
       return (Lam b' (Case (varToCoreExpr b') b (exprType e) [mkAlt DEFAULT [] e' ]))
   | otherwise = do
       e' <- strictifyExpr ss e
       return (Lam b e')
strictifyExpr ss (Cast e c) = do
  e' <- strictifyExpr ss e
  return (Cast e' c)
strictifyExpr ss (Tick t@(SourceNote span _) e) = do
  e' <- strictifyExpr (ss{srcSpan = fromRealSrcSpan span}) e
  return (Tick t e')
strictifyExpr ss (App e1 e2)
  | (checkStrictData ss && not (isType e2) && tcIsLiftedTypeKind(typeKind (exprType e2))
        && not (isStrict (exprType e2))) = do
      (printMessage SevWarning (srcSpan ss)
         (text "The use of lazy type " <> ppr (exprType e2) <> " may lead to memory leaks"))
      e1' <- strictifyExpr ss{checkStrictData = False} e1
      e2' <- strictifyExpr ss{checkStrictData = False} e2
      return (App e1' e2')
  | otherwise = do
      e1' <- strictifyExpr ss e1
      e2' <- strictifyExpr ss e2
      return (App e1' e2')
strictifyExpr _ss e = return e
