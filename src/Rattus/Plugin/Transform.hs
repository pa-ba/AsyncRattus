module Rattus.Plugin.Transform (
    transform
) where

import GHC.Core.Opt.Monad
import GHC.Plugins
import Rattus.Plugin.PrimExpr
import Rattus.Plugin.Utils
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
  Just (DelayApp _ v t) -> do
    bigDelayVar <- bigDelay
    inputValueV <- inputValueVar
    let inputValueType = mkTyConTy inputValueV --Change name of variable
    let inputValueType' = mkAppTy inputValueType v
    inpVar <- mkSysLocalM (fsLit "inpV") inputValueType' inputValueType'
    --let inpVarR = lazySetIdInfo inpVar vanillaIdInfo -- Unsure about this - we convert this to a real var with idInfo
    let ctx' = ctx {fresh = Just inpVar}
    (newExpr, maybePrimInfo) <- transform' ctx' e'
    putMsg $ ppr newExpr
    let primInfo = fromJust maybePrimInfo
    let lambdaExpr = Lam inpVar newExpr
    clockCode <- constructClockExtractionCode v primInfo
    return (App (App (App (App (Var bigDelayVar) (Type v)) (Type t)) clockCode) lambdaExpr, primInfo)
  Just primInfo -> do
        --fatalErrorMsgS "CANNOT TRANSFORM NON PRIMITIVES" 
        error $ showSDocUnsafe $ text "transformPrim: Cannot transform " <> ppr (prim primInfo)
  Nothing -> error "Cannot transform non-prim applications"
transformPrim _ _ = do
  --fatalErrorMsgS "CANNOT TRANSFORM ANYTHING ELSE THAN PRIM EXPRESSIONS"
  error "Cannot transform anything else than prim applications"


transform :: CoreExpr -> CoreM CoreExpr
transform expr = fst <$> transform' emptyCtx expr

transform' :: Ctx -> CoreExpr -> CoreM (CoreExpr, Maybe PrimInfo)
transform' ctx expr@(App e e') = case isPrimExpr expr of
    Just (BoxApp _) -> do
        (newExpr, primInfo) <- transform' ctx e'
        return (App e newExpr, primInfo)
    Just (ArrApp _) -> do
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
transform' ctx (Cast e _) = transform' ctx e
transform' ctx (Tick _ e) = transform' ctx e
transform' _ e = return (e, Nothing)

constructClockExtractionCode :: Type -> PrimInfo -> CoreM CoreExpr
constructClockExtractionCode vt (AdvApp _ arg) = createClockCode vt arg
constructClockExtractionCode vt (SelectApp _ arg arg2) =
    clockUnion vt arg arg2
constructClockExtractionCode _ primInfo = error $ "Cannot construct clock for prim " ++ showSDocUnsafe (ppr (prim primInfo))

-- takes as args: value type, later type, var we adv on
createClockCode :: Type -> (Var, Type) -> CoreM CoreExpr
createClockCode vt (argV, argT) = do
    extractClock <- extractClockVar
    return $ App (App (App (Var extractClock) (Type vt)) (Type argT)) (Var argV)

-- Generate code for union of two clocks.
-- clockUnion (aVar, aType) (bVar, bType) returns the AST for:
--  Set.union (extractClock aVar) (extractClock bVar)
clockUnion :: Type -> (Var,Type) -> (Var, Type) -> CoreM CoreExpr
clockUnion vt arg1 arg2 = do
    unionVar' <- unionVar
    ordInt <- ordIntClass
    clockUnion' unionVar' ordInt vt arg1 arg2

clockUnion' :: Var -> Var -> Type -> (Var,Type) -> (Var, Type) -> CoreM CoreExpr
clockUnion' unionVar ordInt vt arg arg2 = do
    clock1Code <- createClockCode vt arg
    clock2Code <- createClockCode vt arg2
    return $
        App
        (
            App
            (
                App
                (
                    App (Var unionVar) (Type intTy)
                )
                (Var ordInt)
            )
            clock1Code
        )
        clock2Code
