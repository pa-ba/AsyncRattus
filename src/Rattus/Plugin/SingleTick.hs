-- | This module implements the translation from the multi-tick
-- calculus to the single tick calculus.

{-# LANGUAGE CPP #-}

module Rattus.Plugin.SingleTick
  (toSingleTick) where

#if __GLASGOW_HASKELL__ >= 900
import GHC.Plugins
#else
import GhcPlugins
#endif

  
import Rattus.Plugin.Utils
import Prelude hiding ((<>))
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Trans.Class
import Data.List

-- | Transform the given expression from the multi-tick calculus into
-- the single tick calculus form.
toSingleTick :: CoreExpr -> CoreM CoreExpr
toSingleTick (Let (Rec bs) e) = do
  e' <- toSingleTick e
  bs' <- mapM (mapM toSingleTick) bs
  return (Let (Rec bs') e')
toSingleTick (Let (NonRec b e1) e2) = do
  e1' <- toSingleTick e1
  e2' <- toSingleTick e2
  return (Let (NonRec b e1') e2')
toSingleTick (Case e b ty alts) = do
  e' <- toSingleTick e
  alts' <- mapM ((\ (c,bs,f) -> fmap (\ x -> mkAlt c bs x) (toSingleTick f)) . getAlt) alts
  return (Case e' b ty alts')
toSingleTick (Cast e c) = do
  e' <- toSingleTick e
  return (Cast e' c)
toSingleTick (Tick t e) = do
  e' <- toSingleTick e
  return (Tick t e')
toSingleTick (Lam x e) = do
  (e', advs) <- runWriterT (extractAdv' e)
  advs' <- mapM (\ (x,a,b) -> fmap (\b' -> (x,a,b')) (toSingleTick b)) advs
  return (foldLets' advs' (Lam x e'))
toSingleTick (App e1 e2)
  | isDelayApp e1 = do
      (e2', advs) <- runWriterT (extractAdv e2)
      advs' <- mapM (mapM toSingleTick) advs
      return (foldLets advs' (App e1 e2'))
  | otherwise = do
      e1' <- toSingleTick e1
      e2' <- toSingleTick e2
      return (App e1' e2')

toSingleTick e@Type{} = return e
toSingleTick e@Var{} = return e
toSingleTick e@Lit{} = return e
toSingleTick e@Coercion{} = return e

foldLets :: [(Id,CoreExpr)] -> CoreExpr -> CoreExpr
foldLets ls e = foldl' (\e' (x,b) -> Let (NonRec x b) e') e ls

foldLets' :: [(Id,CoreExpr,CoreExpr)] -> CoreExpr -> CoreExpr
foldLets' ls e = foldl' (\e' (x,a,b) -> Let (NonRec x (App a b)) e') e ls

extractAdvApp :: CoreExpr -> CoreExpr -> WriterT [(Id,CoreExpr)] CoreM CoreExpr
extractAdvApp e1 e2
  | isVar e2 = return (App e1 e2)
  | otherwise = do
  x <- lift (mkSysLocalFromExpr (fsLit "adv") e2)
  tell [(x,e2)]
  return (App e1 (Var x))

-- This is used to pull adv out of delayed terms. The writer monad
-- returns mappings from fresh variables to terms that occur as
-- argument of adv.
-- 
-- That is, occurrences of @adv t@ are replaced with @adv x@ (for some
-- fresh variable @x@) and the pair @(x,t)@ is returned in the
-- writer monad.
extractAdv :: CoreExpr -> WriterT [(Id,CoreExpr)] CoreM CoreExpr
extractAdv e@(App e1 e2)
  | isAdvApp e1 = extractAdvApp e1 e2
  | isDelayApp e1 = do
      (e2', advs) <- lift $ runWriterT (extractAdv e2)
      advs' <- mapM (mapM extractAdv) advs
      return (foldLets advs' (App e1 e2'))
  | isBoxApp e1 = lift $ toSingleTick e
  | otherwise = do
      e1' <- extractAdv e1
      e2' <- extractAdv e2
      return (App e1' e2')
extractAdv (Lam x e) = do
  (e', advs) <- lift $ runWriterT (extractAdv' e)
  advs' <- mapM (\ (x,a,b) -> fmap (\b' -> (x,b')) (extractAdvApp a b)) advs
  return (foldLets advs' (Lam x e'))
extractAdv (Case e b ty alts) = do
  e' <- extractAdv e
  alts' <- mapM ((\ (c,bs,f) -> fmap (\ x -> mkAlt c bs x) (extractAdv f)) . getAlt) alts
  return (Case e' b ty alts')
extractAdv (Cast e c) = do
  e' <- extractAdv e
  return (Cast e' c)
extractAdv (Tick t e) = do
  e' <- extractAdv e
  return (Tick t e')
extractAdv e@(Let Rec{} _) = lift $ toSingleTick e
extractAdv (Let (NonRec b e1) e2) = do
  e1' <- extractAdv e1
  e2' <- extractAdv e2
  return (Let (NonRec b e1') e2')
extractAdv e@Type{} = return e
extractAdv e@Var{} = return e
extractAdv e@Lit{} = return e
extractAdv e@Coercion{} = return e

-- This is used to pull adv out of lambdas. The writer monad returns
-- mappings from fresh variables to occurrences of adv and the term it
-- is applied to.
-- 
-- That is occurrences of @adv t@ are replaced with a fresh variable
-- @x@ and the triple @(x,adv,t)@ is returned in the writer monad.
extractAdv' :: CoreExpr -> WriterT [(Id,CoreExpr,CoreExpr)] CoreM CoreExpr
extractAdv' e@(App e1 e2)
  | isAdvApp e1 = do
       x <- lift (mkSysLocalFromExpr (fsLit "adv") e)
       tell [(x,e1,e2)]
       return (Var x)
  | isDelayApp e1 = do
      (e2', advs) <- lift $ runWriterT (extractAdv e2)
      advs' <- mapM (mapM extractAdv') advs
      return (foldLets advs' (App e1 e2'))
  | isBoxApp e1 = lift $ toSingleTick e
  | otherwise = do
      e1' <- extractAdv' e1
      e2' <- extractAdv' e2
      return (App e1' e2')
extractAdv' (Lam x e) = do
  e' <- extractAdv' e
  return (Lam x e')
extractAdv' (Case e b ty alts) = do
  e' <- extractAdv' e
  alts' <- mapM ((\ (c,bs,f) -> fmap (\ x -> mkAlt c bs x) (extractAdv' f)) . getAlt) alts
  return (Case e' b ty alts')
extractAdv' (Cast e c) = do
  e' <- extractAdv' e
  return (Cast e' c)
extractAdv' (Tick t e) = do
  e' <- extractAdv' e
  return (Tick t e')
extractAdv' e@(Let Rec{} _) = lift $ toSingleTick e
extractAdv' (Let (NonRec b e1) e2) = do
  e1' <- extractAdv' e1
  e2' <- extractAdv' e2
  return (Let (NonRec b e1') e2')
extractAdv' e@Type{} = return e
extractAdv' e@Var{} = return e
extractAdv' e@Lit{} = return e
extractAdv' e@Coercion{} = return e



isDelayApp :: CoreExpr -> Bool
isDelayApp = isPrimApp (\occ -> occ == "delay")

isBoxApp :: CoreExpr -> Bool
isBoxApp = isPrimApp (\occ -> occ == "Box" || occ == "box")

isAdvApp :: CoreExpr -> Bool
isAdvApp = isPrimApp (\occ -> occ == "adv")


isPrimApp :: (String -> Bool) -> CoreExpr -> Bool
isPrimApp p (App e e')
  | isType e' || not  (tcIsLiftedTypeKind(typeKind (exprType e'))) = isPrimApp p e
  | otherwise = False
isPrimApp p (Cast e _) = isPrimApp p e
isPrimApp p (Tick _ e) = isPrimApp p e
isPrimApp p (Var v) = isPrimVar p v
isPrimApp _ _ = False

isPrimVar :: (String -> Bool) -> Var -> Bool
isPrimVar p v = maybe False id $ do
  let name = varName v
  mod <- nameModule_maybe name
  let occ = getOccString name
  return (p occ
          && ((moduleNameString (moduleName mod) == "Rattus.Internal") ||
          moduleNameString (moduleName mod) == "Rattus.Primitives"))
