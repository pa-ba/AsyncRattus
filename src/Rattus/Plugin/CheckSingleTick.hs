{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

-- | This module implements the check that the transformed code is
-- typable in the single tick calculus.

module Rattus.Plugin.CheckSingleTick
  (checkExpr, CheckExpr (..)) where



import GHC.Plugins




import Rattus.Plugin.Utils
import qualified Rattus.Plugin.PrimExpr as Prim
import Prelude hiding ((<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Control.Monad (foldM)
import GHC.Types.Tickish
import Control.Applicative ((<|>))

type LCtx = Set Var
data HiddenReason = BoxApp | AdvApp | NestedRec Var | FunDef | DelayApp
type Hidden = Map Var HiddenReason


data TypeError = TypeError SrcSpan SDoc


data Ctx = Ctx
  { current :: LCtx,
    hidden :: Hidden,
    earlier :: Maybe LCtx,
    srcLoc :: SrcSpan,
    recDef :: Set Var, -- ^ recursively defined variables 
    stableTypes :: Set Var,
    allowRecursion :: Bool,
    --inDelay :: Bool,
    hasSeenAdvSelect :: Bool,
    fresh :: Maybe Var
    }

hasTick :: Ctx -> Bool
hasTick = isJust . earlier


stabilize :: HiddenReason -> Ctx -> Ctx
stabilize hr c = c
  {current = Set.empty,
   earlier = Nothing,
   hidden = hidden c `Map.union` Map.fromSet (const hr) ctxHid
  }
  where ctxHid = maybe (current c) (Set.union (current c)) (earlier c)


data Scope = Hidden SDoc | Visible

getScope  :: Ctx -> Var -> Scope
getScope c v =
    if v `Set.member` recDef c then
      if hasTick c || allowRecursion c then Visible
      else Hidden ("(Mutually) recursive call to " <> ppr v <> " must occur under delay")
    else case Map.lookup v (hidden c) of
      Just reason ->
        if (isStable (stableTypes c) (varType v)) then Visible
        else case reason of
          NestedRec rv ->
            if allowRecursion c then Visible
            else Hidden ("Variable " <> ppr v <> " is no longer in scope:"
                         $$ "It appears in a local recursive definition (namely of " <> ppr rv <> ")"
                         $$ "and is of type " <> ppr (varType v) <> ", which is not stable.")
          BoxApp -> Hidden ("Variable " <> ppr v <> " is no longer in scope:" $$
                       "It occurs under " <> keyword "box" $$ "and is of type " <> ppr (varType v) <> ", which is not stable.")
          AdvApp -> Hidden ("Variable " <> ppr v <> " is no longer in scope: It occurs under adv.")

          FunDef -> Hidden ("Variable " <> ppr v <> " is no longer in scope: It occurs in a function that is defined under a delay, is a of a non-stable type " <> ppr (varType v) <> ", and is bound outside delay")
          DelayApp -> Hidden ("Variable " <> ppr v <> " is no longer in scope: It occurs under two occurrences of delay and is a of a non-stable type " <> ppr (varType v))
      Nothing
          | maybe False (Set.member v) (earlier c) ->
            if isStable (stableTypes c) (varType v) then Visible
            else Hidden ("Variable " <> ppr v <> " is no longer in scope:" $$
                         "It occurs under delay" $$ "and is of type " <> ppr (varType v) <> ", which is not stable.")
          | Set.member v (current c) -> Visible
          | otherwise -> Visible



pickFirst :: SrcSpan -> SrcSpan -> SrcSpan
pickFirst s@RealSrcSpan{} _ = s
pickFirst _ s = s

typeError :: Ctx -> Var -> SDoc -> TypeError
typeError ctx var = TypeError (pickFirst (srcLoc ctx) (nameSrcSpan (varName var)))

emptyCtx :: CheckExpr -> Ctx
emptyCtx c =
  Ctx { current =  Set.empty,
        earlier = Nothing,
        hidden = Map.empty,
        srcLoc = noLocationInfo,
        recDef = recursiveSet c,
        stableTypes = Set.empty,
        allowRecursion = allowRecExp c,
        -- inDelay = False,
        hasSeenAdvSelect = False,
        fresh = Nothing
        }

inDelay :: Ctx -> Bool
inDelay = isJust . earlier

stabilizeLater :: Ctx -> Ctx
stabilizeLater c =
  case earlier c of
    Just earl -> c {earlier = Nothing,
                    hidden = hidden c `Map.union` Map.fromSet (const FunDef) earl}
    Nothing -> c

isStableConstr :: Type -> CoreM (Maybe Var)
isStableConstr t =
  case splitTyConApp_maybe t of
    Just (con,[args]) ->
      case getNameModule con of
        Just (name, mod) ->
          if isRattModule mod && name == "Stable"
          then return (getTyVar_maybe args)
          else return Nothing
        _ -> return Nothing
    _ ->  return Nothing


data SymbolicClock = Clock Var | Union SymbolicClock SymbolicClock

instance Show SymbolicClock where
  show (Clock v) = "Clock " ++ (showSDocUnsafe . ppr) v
  show (Union c1 c2) = "Union (" ++ show c1 ++ ") (" ++ show c2 ++ ")"

newtype CheckResult = CheckResult{
  advSelect :: Maybe Var
}

emptyCheckResult :: CheckResult
emptyCheckResult = CheckResult {advSelect = Nothing}

data CheckExpr = CheckExpr{
  recursiveSet :: Set Var,
  oldExpr :: Expr Var,
  fatalError :: Bool,
  verbose :: Bool,
  allowRecExp :: Bool
  }

checkExpr :: CheckExpr -> Expr Var -> CoreM Bool
checkExpr c e = do
  res <- checkExpr' (emptyCtx c) e
  case res of
    Right _ -> return True
    Left (TypeError src doc) ->
      let sev = if fatalError c then SevError else SevWarning
      in if verbose c then do
        printMessage sev src ("Internal error in Rattus Plugin: single tick transformation did not preserve typing." $$ doc)
        liftIO $ putStrLn "-------- old --------"
        liftIO $ putStrLn (showSDocUnsafe (ppr (oldExpr c)))
        liftIO $ putStrLn "-------- new --------"
        liftIO $ putStrLn (showSDocUnsafe (ppr e))
        return $ not (fatalError c)
         else do
        printMessage sev noSrcSpan ("Internal error in Rattus Plugin: single tick transformation did not preserve typing." $$
                             "Compile with flags \"-fplugin-opt Rattus.Plugin:debug\" and \"-g2\" for detailed information")
        return $ not (fatalError c)
        

checkExpr' :: Ctx -> Expr Var -> CoreM (Either TypeError CheckResult)
checkExpr' c (App e e') | isType e' || (not $ tcIsLiftedTypeKind $ typeKind $ exprType e')
  = checkExpr' c e
checkExpr' c@Ctx{current = cur} expr@(App e e') =
  case Prim.isPrimExpr expr of
    Just (Prim.BoxApp _) ->
      checkExpr' (stabilize BoxApp c) e'
    Just (Prim.ArrApp _) ->
      checkExpr' (stabilize BoxApp c) e'
    Just (Prim.DelayApp f) ->
      if inDelay c then return $ Left $ typeError c f (text "Nested delays not allowed")
      else checkExpr' c{current = Set.empty, earlier = Just cur} e'
    Just (Prim.AdvApp f _)  -> checkAdvSelect c Prim.Adv f
    Just (Prim.SelectApp f _ _)-> checkAdvSelect c Prim.Select f
    Nothing -> checkBoth c e e'
checkExpr' c (Case e _ _ [Alt DEFAULT [] rhs]) = checkBoth c e rhs
checkExpr' c (Case e v _ alts) = do
    res <- checkExpr' c' e
    let maybePrimVar = foldl (\acc (Alt _ _ altE) -> acc <|> recursiveIsPrimExpr altE) Nothing alts
    case maybePrimVar of
      Just _ -> return $ Left $ typeError c v "Primitives in case expressions are not allowed"
      Nothing -> return res
  where c' = addVars [v] c
checkExpr' c (Lam v e)
  | isTyVar v || (not $ tcIsLiftedTypeKind $ typeKind $ varType v) = do
      is <- isStableConstr (varType v)
      let c' = case is of
            Nothing -> c
            Just t -> c{stableTypes = Set.insert t (stableTypes c)}
      checkExpr' c' e
  | otherwise = checkExpr' (addVars [v] (stabilizeLater c)) e
checkExpr' _ (Type _)  = return $ Right emptyCheckResult
checkExpr' _ (Lit _)  = return $ Right emptyCheckResult
checkExpr' _ (Coercion _)  = return $ Right emptyCheckResult
checkExpr' c (Tick (SourceNote span _name) e) =
  checkExpr' c{srcLoc = fromRealSrcSpan span} e
checkExpr' c (Tick _ e) = checkExpr' c e
checkExpr' c (Cast e _) = checkExpr' c e
checkExpr' c (Let (NonRec v e1) e2) = do
  c' <- checkAndUpdate c e1
  case c' of
    Left err -> return $ Left err
    Right ctx -> checkExpr' (addVars [v] ctx) e2
checkExpr' _ (Let (Rec ([])) _) = return $ Right emptyCheckResult
checkExpr' c (Let (Rec binds) e2) = do
    r1 <- mapM (\ (v,e) -> checkExpr' (c' v) e) binds
    ctxE <- foldM (\acc r ->
      case (acc, r) of
        (_, Left err) -> return $ Left err
        (Left err, _) -> return $ Left err
        (Right res, Right res') -> return $ updateCtxFromResult res res'
        ) (Right c) r1
    case ctxE of
      Left err -> return $ Left err
      Right ctx -> checkExpr' (addVars vs ctx) e2
  where vs = map fst binds
        ctxHid = maybe (current c) (Set.union (current c)) (earlier c)
        c' v = c {current = Set.empty,
                  earlier = Nothing,
                  hidden =  hidden c `Map.union`
                   (Map.fromSet (const (NestedRec v)) ctxHid),
                  recDef = recDef c `Set.union` Set.fromList vs }
checkExpr' c  (Var v)
  | tcIsLiftedTypeKind $ typeKind $ varType v =  case getScope c v of
             Hidden reason -> return $ Left $ typeError c v reason
             Visible -> return $ Right emptyCheckResult
  | otherwise = return $ Right emptyCheckResult

-- Assumes that Prim is either adv or select.
checkAdvSelect :: Ctx ->Prim.Prim -> Var -> CoreM (Either TypeError CheckResult)
checkAdvSelect c p f =
  -- We only allow adv/select to be applied to variables.
  -- But there is no reason to check whether the arguments are variables, since this is ensured by isPrimExpr.
  if not $ inDelay c then return $ Left $ typeError c f (text "can only use " <> ppr p <> text " under delay")
  else if hasSeenAdvSelect c then return $ Left $ typeError c f (text "Only one " <> ppr p <> text " allowed in a delay")
  else return $ Right $ emptyCheckResult {advSelect = Just f}


recursiveIsPrimExpr :: CoreExpr -> Maybe Var
recursiveIsPrimExpr expr@(App e e') =
  case Prim.isPrimExpr expr of
    Just primInfo -> Just $ Prim.function primInfo  
    Nothing -> recursiveIsPrimExpr e <|> recursiveIsPrimExpr e'
recursiveIsPrimExpr (Lam _ e) = recursiveIsPrimExpr e
recursiveIsPrimExpr (Tick _ e) = recursiveIsPrimExpr e
recursiveIsPrimExpr (Cast e _) = recursiveIsPrimExpr e
recursiveIsPrimExpr (Let (NonRec _ e) e') = recursiveIsPrimExpr e <|> recursiveIsPrimExpr e'
recursiveIsPrimExpr (Let (Rec binds) e) = foldl (\acc (_, e) -> acc <|> recursiveIsPrimExpr e) Nothing binds
recursiveIsPrimExpr (Case e _ _ alts) = recursiveIsPrimExpr e <|> foldl (\acc (Alt _ _ e') -> acc <|> recursiveIsPrimExpr e') Nothing alts
recursiveIsPrimExpr _ = Nothing



addVars :: [Var] -> Ctx -> Ctx
addVars v c = c{current = Set.fromList v `Set.union` current c }

checkBoth :: Ctx -> CoreExpr -> CoreExpr -> CoreM (Either TypeError CheckResult)
checkBoth c e e' = do
  c' <- checkAndUpdate c e
  case c' of
    Left err -> return $ Left err
    Right ctx -> checkExpr' ctx e'

updateCtxFromResult :: Ctx -> CheckResult -> Either TypeError Ctx
updateCtxFromResult c@(Ctx {hasSeenAdvSelect = True}) (CheckResult {advSelect = Just v}) = Left $ typeError c v "Only one adv/select allowed in a delay"
updateCtxFromResult c@(Ctx {hasSeenAdvSelect = hasSeen}) r = Right $ c {hasSeenAdvSelect = hasSeen || isJust (advSelect r)}

checkAndUpdate :: Ctx -> Expr Var -> CoreM (Either TypeError Ctx)
checkAndUpdate c e = do
  res <- checkExpr' c e
  case res of
    Left err -> return $ Left err
    Right r -> return $ updateCtxFromResult c r

{-
countAdvSelect' :: Ctx -> Expr Var -> Either String CheckResult
countAdvSelect' ctx (App e e') = case isPrimExpr ctx e of
      Just (p, _) -> case D.trace ("we have met a prim: " ++ showSDocUnsafe (ppr p)) p of
        Adv | not (isVar e') -> Left "Can only adv on variables"
            | hasSeenAdvSelect ctx -> Left "Only one adv/select allowed in a delay"
            | otherwise -> Right CheckResult { foundClock = Just (Clock (getVar e')) }
        Select -> Left "Select not implemented"
                    -- | not $ all isVar args -> Left "Can only select on variables"
                    -- | hasSeenAdvSelect ctx -> Left "Only one adv/select allowed in a delay"
                    -- | otherwise -> Right CheckResult { foundClock = let [first, second] = map (Clock . getVar) args in Just $ Union first second}
        Delay | inDelay ctx -> Left "Nested delays not allowed"
              | otherwise -> case countAdvSelect' (ctx {inDelay = True}) e' of
                Right r | isJust (foundClock r) -> D.trace ("Delay: found correct clock " ++ show r) (Right r)
                Left s -> Left s
                _ -> Left "Each delay must contain an adv or select"
        _ -> Right emptyCheckResult   -- what about box/unbox?
      _ -> case checkAndUpdate ctx e of
          Left s -> Left s
          Right ctx' -> countAdvSelect' ctx' e'
countAdvSelect' ctx (Lam _ rhs) = countAdvSelect' ctx rhs
countAdvSelect' ctx (Let (NonRec _ e') e) =
  case fmap (updateCtxFromResult ctx) (countAdvSelect' ctx e) of
        Left s -> Left s
        Right ctx' -> countAdvSelect' ctx' e'
countAdvSelect' ctx (Case e _ _ alts) = case countAdvSelect' ctx e of
  Left s -> Left s
  Right res -> snd <$> foldM (\(c, r) (Alt _ _ e') ->
    case countAdvSelect' c e' of
      Left s -> Left s
      Right r' | advSelect r && advSelect r' -> Left "Only one adv/select allowed in a delay"
      Right r' -> Right (updateCtxFromResult c r', r')) (updateCtxFromResult ctx res, res) alts
countAdvSelect' ctx (Cast e _) = countAdvSelect' ctx e
countAdvSelect' ctx (Tick _ e) = countAdvSelect' ctx e
countAdvSelect' _ _ = Right emptyCheckResult
-}