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
  Just primInfo@(PrimInfo {prim = Adv, function = f}) -> do
    varAdv' <- adv'Var
    let newE = replaceVar f varAdv' e
    return (App (App newE e') (Var (fromJust $ fresh ctx)), primInfo)
  Just primInfo@(PrimInfo {prim = Select, function = f}) -> do
    varSelect' <- select'Var
    let newE = replaceVar f varSelect' e
    return (App (App newE e') (Var (fromJust $ fresh ctx)), primInfo)
  Just (PrimInfo {prim = Delay}) -> do
    bigDelayVar <- bigDelay
    inputValueV <- inputValueVar
    let inputValueType = mkTyConTy inputValueV --Change name of variable
    inpVar <- mkSysLocalM (fsLit "inpV") inputValueType inputValueType
    --let inpVarR = lazySetIdInfo inpVar vanillaIdInfo -- Unsure about this - we convert this to a real var with idInfo
    let ctx' = ctx {fresh = Just inpVar}
    (newExpr, maybePrimInfo) <- transform' ctx' e'
    let primInfo = fromJust maybePrimInfo
    let lambdaExpr = Lam inpVar newExpr
    clockCode <- constructClockExtractionCode primInfo
    return (App (App (Var bigDelayVar) clockCode) lambdaExpr, primInfo)
  Just (PrimInfo {prim = p}) -> do
        --fatalErrorMsgS "CANNOT TRANSFORM NON PRIMITIVES" 
        error $ showSDocUnsafe $ text "transformPrim: Cannot transform " <> ppr p
  Nothing -> error "Cannot transform non-prim applications"
transformPrim _ _ = do
  --fatalErrorMsgS "CANNOT TRANSFORM ANYTHING ELSE THAN PRIM EXPRESSIONS"
  error "Cannot transform anything else than prim applications"


transform :: CoreExpr -> CoreM CoreExpr
transform expr = do 
    (newExpr, _) <- transform' emptyCtx expr
    putMsgS "OLD-AST"
    putMsg (ppr expr)
    putMsgS "NEW AST"
    putMsg (ppr newExpr)
    putMsgS "NEW TREE SHOW"
    putMsgS (showTree newExpr)
    return newExpr
    --fst <$> transform' emptyCtx expr

transform' :: Ctx -> CoreExpr -> CoreM (CoreExpr, Maybe PrimInfo)
transform' ctx expr@(App e e') = case isPrimExpr expr of
    Just (PrimInfo {prim = Adv}) -> do
        (newExpr, primInfo) <- transformPrim ctx expr
        return (newExpr, Just primInfo)
    Just (PrimInfo {prim = Select}) -> do
        (newExpr, primInfo) <- transformPrim ctx expr
        return (newExpr, Just primInfo)
    Just (PrimInfo {prim = Delay}) -> do
        (newExpr, primInfo) <- transformPrim ctx expr
        return (newExpr, Just primInfo)
    Just _ -> do
        (newExpr, primInfo) <- transform' ctx e'
        return (App e newExpr, primInfo)
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
    (expr, primInfo) <- transform' ctx e
    -- Throw away primInfos occurring in alts. This is safe because we
    -- don't allow adv/select in alts.
    alts' <- mapM (\(Alt con binds expr) -> transform' ctx expr <&> (Alt con binds . fst)) alts
    return (Case expr b t alts', primInfo)
transform' ctx (Cast e _) = transform' ctx e
transform' ctx (Tick _ e) = transform' ctx e
transform' _ e = return (e, Nothing)

constructClockExtractionCode :: PrimInfo -> CoreM CoreExpr
constructClockExtractionCode (PrimInfo { prim = Adv, arg = arg }) = createClockCode arg
constructClockExtractionCode (PrimInfo { prim = Select, arg = arg, arg2 = Just arg2}) =
    clockUnion arg arg2
constructClockExtractionCode (PrimInfo { prim = p }) = error $ "Cannot construct clock for prim " ++ showSDocUnsafe (ppr p)

createClockCode :: (Var, Type) -> CoreM CoreExpr
createClockCode (argV, argT) = do
    extractClock <- extractClockVar
    return $ App (App (Var extractClock) (Type argT)) (Var argV)

-- Generate code for union of two clocks.
-- clockUnion (aVar, aType) (bVar, bType) returns the AST for:
--  Set.union (extractClock aVar) (extractClock bVar)
clockUnion :: (Var,Type) -> (Var, Type) -> CoreM CoreExpr
clockUnion arg1 arg2 = do
    unionVar' <- unionVar
    ordInt <- ordIntClass
    clockUnion' unionVar' ordInt arg1 arg2

clockUnion' :: Var -> Var -> (Var,Type) -> (Var, Type) -> CoreM CoreExpr
clockUnion' unionVar ordInt arg arg2 = do
    clock1Code <- createClockCode arg
    clock2Code <- createClockCode arg2
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
