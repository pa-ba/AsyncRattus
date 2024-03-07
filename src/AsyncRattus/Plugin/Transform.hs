{-# LANGUAGE TupleSections #-}
module AsyncRattus.Plugin.Transform (
    transform
) where

import GHC.Core.Opt.Monad
import GHC.Plugins
import AsyncRattus.Plugin.PrimExpr
import AsyncRattus.Plugin.Utils
import Data.Maybe (fromJust)
import Prelude hiding ((<>))
import Data.Functor ((<&>))
import Control.Applicative ((<|>))
import Data.Tuple (swap)

data Ctx = Ctx {
    fresh :: Maybe Var
}

emptyCtx :: Ctx
emptyCtx = Ctx {
    fresh = Nothing
}

replaceVar :: Var -> Var -> Expr Var ->  Expr Var
replaceVar match rep (Var v) = if v == match then Var rep else Var v
replaceVar match rep (App e e') = App (replaceVar match rep e) (replaceVar match rep e')
replaceVar match rep (Tick _ e) = replaceVar match rep e
replaceVar match rep (Lam v e) = Lam (if v == match then rep else v) (replaceVar match rep e)
replaceVar match rep (Let (NonRec b e') e) =
  Let (NonRec newB (replaceVar  match rep e')) (replaceVar match rep e)
  where newB = if b == match then rep else b
replaceVar match rep (Cast e _) = replaceVar match rep e
replaceVar match rep (Case e b t alts) =
  Case newExpr newB t (map (\(Alt con binds expr) -> Alt con (map (\v -> if v == match then rep else v) binds) (replaceVar match rep expr)) alts)
  where newExpr = replaceVar match rep e
        newB = if b == match then rep else b
replaceVar _ _ e = e

transformPrim :: Ctx -> Expr Var -> CoreM (Expr Var, PrimInfo)
transformPrim ctx expr@(App e e') = case isPrimExpr expr of
  Just primInfo@(AdvApp f _) -> do
    varAdv' <- adv'Var
    let newE = replaceVar f varAdv' e
    return (App (App newE e') (Var (fromJust $ fresh ctx)), primInfo)
  Just primInfo@(SelectApp f _ _) -> do
    varSelect' <- select'Var
    let newE = replaceVar f varSelect' e
    return (App (App newE e') (Var (fromJust $ fresh ctx)), primInfo)
  Just (DelayApp _ t) -> do
    bigDelayVar <- bigDelay
    inputValueV <- inputValueVar
    let inputValueType = mkTyConTy inputValueV 
    inpVar <- mkSysLocalM (fsLit "inpV") inputValueType inputValueType
    let ctx' = ctx {fresh = Just inpVar}
    (newExpr, maybePrimInfo) <- transform' ctx' e'
    let primInfo = fromJust maybePrimInfo
    let lambdaExpr = Lam inpVar newExpr
    clockCode <- constructClockExtractionCode primInfo
    return (App (App (App (Var bigDelayVar) (Type t)) clockCode) lambdaExpr, primInfo)
  Just primInfo -> do
        error $ showSDocUnsafe $ text "transformPrim: Cannot transform " <> ppr (prim primInfo)
  Nothing -> error "Cannot transform non-prim applications"
transformPrim _ _ = do
  error "Cannot transform anything else than prim applications"


transform :: CoreExpr -> CoreM CoreExpr
transform expr = fst <$> transform' emptyCtx expr

transform' :: Ctx -> CoreExpr -> CoreM (CoreExpr, Maybe PrimInfo)
transform' ctx expr@(App e e') = case isPrimExpr expr of
    Just (BoxApp _) -> do
        (newExpr, primInfo) <- transform' ctx e'
        return (App e newExpr, primInfo)
    Just _ -> do
        (newExpr, primInfo) <- transformPrim ctx expr
        return (newExpr, Just primInfo)
    Nothing -> do
        (newExpr, primInfo) <- transform' ctx e
        (newExpr', primInfo') <- transform' ctx e'
        return (App newExpr newExpr', primInfo <|> primInfo')
transform' ctx (Lam b rhs) = do
    (newExpr, primInfo) <- transform' ctx rhs
    return (Lam b newExpr, primInfo)
transform' ctx (Let (NonRec b rhs) e) = do
    (newRhs, primInfo) <- transform' ctx rhs
    (newExpr, primInfo') <- transform' ctx e
    return (Let (NonRec b newRhs) newExpr, primInfo <|> primInfo')
transform' ctx (Let (Rec binds) e) = do
    transformedBinds <- mapM (\(b, bindE) -> fmap (b,) (transform' ctx bindE)) binds
    (e', mPi) <- transform' ctx e
    let primInfos = map (\(_, (_, p)) -> p) transformedBinds
    let firstPrimInfo = foldl (<|>) mPi primInfos
    newBinds <- mapM (\(b, (e, _)) -> return (b, e)) transformedBinds
    return (Let (Rec newBinds) e', firstPrimInfo)
transform' ctx (Case e b t alts) = do
    -- The checking pass has ensured that there are not advances on different
    -- clocks. Thus we can just pick the first PrimInfo we find.
    (expr, primInfo) <- transform' ctx e

    -- For each alternative, transform it and save the maybePrimInfo-value
    transformed <- mapM (\(Alt con binds expr) -> transform' ctx expr <&> fmap (Alt con binds) . swap) alts

    -- Of all the primInfos we have, pick the first one. This is safe because
    -- the checking pass has ensured that the clocks of all primitives.
    let firstPrimInfo = foldl (\acc (p, _) -> acc <|> p) primInfo transformed
    let alts' = map snd transformed
    return (Case expr b t alts', firstPrimInfo)
transform' ctx (Cast e c) = do (e' , p) <- transform' ctx e; return (Cast e' c, p)
transform' ctx (Tick t e) = do (e' , p) <- transform' ctx e; return (Tick t e', p)
transform' _ e = return (e, Nothing)

constructClockExtractionCode :: PrimInfo -> CoreM CoreExpr
constructClockExtractionCode (AdvApp _ arg) = createClockCode arg
constructClockExtractionCode (SelectApp _ arg arg2) =
    clockUnion arg arg2
constructClockExtractionCode primInfo = error $ "Cannot construct clock for prim " ++ showSDocUnsafe (ppr (prim primInfo))


createClockCode :: (Var, Type) -> CoreM CoreExpr
createClockCode (argV, argT) = do
    extractClock <- extractClockVar
    return $ App (App (Var extractClock) (Type argT)) (Var argV)

-- Generate code for union of two clocks.
-- clockUnion (aVar, aType) (bVar, bType) returns the AST for:
--  Set.union (extractClock aVar) (extractClock bVar)

clockUnion :: (Var,Type) -> (Var, Type) -> CoreM CoreExpr
clockUnion arg arg2 = do
    clock1Code <- createClockCode arg
    clock2Code <- createClockCode arg2
    unionVar' <- unionVar
    return $
        App
        (
            App
            (
                   (Var unionVar')
            )
            clock1Code
        )
        clock2Code
