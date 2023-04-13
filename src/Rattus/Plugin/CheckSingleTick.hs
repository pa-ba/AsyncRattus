{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

-- | This module implements the check that the transformed code is
-- typable in the single tick calculus.

module Rattus.Plugin.CheckSingleTick
  (checkExpr, CheckExpr (..)) where


import GHC.Types.Tickish



import GHC.Plugins




import Rattus.Plugin.Utils

import Prelude hiding ((<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, fromJust)
import Data.Functor ((<&>))

type LCtx = Set Var
data HiddenReason = BoxApp | AdvApp | NestedRec Var | FunDef | DelayApp
type Hidden = Map Var HiddenReason

data Prim = Delay | Adv | Box | Arr | Select

data PartialPrimInfo = PartialPrimInfo {
  primPart :: Prim,
  functionPart :: Var,
  argTypePart :: Maybe Type,
  argVarPart :: Maybe Var,
  arg2TypePart :: Maybe Type,
  arg2VarPart :: Maybe Var
}

data PrimInfo = PrimInfo {
  prim :: Prim,
  function :: Var,
  arg :: (Var, Type),
  arg2 :: Maybe (Var, Type)
}

data TypeError = TypeError SrcSpan SDoc

instance Outputable PartialPrimInfo where
  ppr (PartialPrimInfo prim f argT argV arg2T arg2V) = text "PartialPrimInfo { prim = " <> ppr prim <> text ", function = " <> ppr f <> text ", argT = " <> ppr argT <> text ", argV = " <> ppr argV <> text ", arg2T = " <> ppr arg2T <> ", arg2V = " <> ppr arg2V

instance Outputable Prim where
  ppr Delay = "delay"
  ppr Adv = "adv"
  ppr Select = "select"
  ppr Box = "box"
  ppr Arr = "arr"

data Ctx = Ctx
  { current :: LCtx,
    hidden :: Hidden,
    earlier :: Maybe LCtx,
    srcLoc :: SrcSpan,
    recDef :: Set Var, -- ^ recursively defined variables 
    stableTypes :: Set Var,
    primAlias :: Map Var Prim,
    allowRecursion :: Bool,
    inDelay :: Bool,
    hasSeenAdvSelect :: Bool,
    fresh :: Maybe Var
    }

hasTick :: Ctx -> Bool
hasTick = isJust . earlier

primMap :: Map FastString Prim
primMap = Map.fromList
  [("delay", Delay),
   ("adv", Adv),
   ("select", Select),
   ("box", Box),
   ("arr", Arr)
   ]


isPrim :: Ctx -> Var -> Maybe Prim
isPrim c v
  | Just p <- Map.lookup v (primAlias c) = Just p
  | otherwise = do
  (name,mod) <- getNameModule v
  if isRattModule mod then Map.lookup name primMap
  else Nothing



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


typeError :: Ctx -> Var -> SDoc -> CoreM (Maybe TypeError)
typeError ctx var doc =
  return (Just (mkTypeError ctx var doc))

mkTypeError :: Ctx -> Var -> SDoc -> TypeError
mkTypeError ctx var = TypeError (pickFirst (srcLoc ctx) (nameSrcSpan (varName var)))

emptyCtx :: CheckExpr -> Ctx
emptyCtx c =
  Ctx { current =  Set.empty,
        earlier = Nothing,
        hidden = Map.empty,
        srcLoc = noLocationInfo,
        recDef = recursiveSet c,
        primAlias = Map.empty,
        stableTypes = Set.empty,
        allowRecursion = allowRecExp c,
        inDelay = False,
        hasSeenAdvSelect = False,
        fresh = Nothing
        }

createPartialPrimInfo :: Prim -> Var -> PartialPrimInfo
createPartialPrimInfo prim function =
  PartialPrimInfo {
    primPart = prim,
    functionPart = function,
    argTypePart = Nothing,
    argVarPart = Nothing,
    arg2TypePart = Nothing,
    arg2VarPart = Nothing
  }

{-
isPrimExpr :: Ctx -> Expr Var -> Maybe (Prim,Var)
isPrimExpr c (App e (Type _)) = isPrimExpr c e
isPrimExpr c (App e e') | not $ tcIsLiftedTypeKind $ typeKind $ exprType e' = isPrimExpr c e
isPrimExpr c (Var v) = fmap (,v) (isPrim c v)
isPrimExpr c (Tick _ e) = isPrimExpr c e
isPrimExpr c (Lam v e)
  | isTyVar v || (not $ tcIsLiftedTypeKind $ typeKind $ varType v) = isPrimExpr c e
isPrimExpr _ _ = Nothing
-}

validatePartialPrimInfo :: PartialPrimInfo -> Maybe PrimInfo
validatePartialPrimInfo (PartialPrimInfo Select f (Just argT) (Just argV) (Just arg2T) (Just arg2V)) = Just PrimInfo { prim = Select, function = f, arg = (argV, argT), arg2 = Just (arg2V, arg2T)}
validatePartialPrimInfo (PartialPrimInfo {primPart = Delay, functionPart = f, argTypePart = Just typ}) = Just PrimInfo {prim = Delay, function = f, arg = (undefined, typ), arg2 = Nothing}    -- UGLY HACK (connected to the one below)
validatePartialPrimInfo (PartialPrimInfo p f (Just argT) (Just argV) Nothing Nothing) = Just PrimInfo { prim = p, function = f, arg = (argV, argT), arg2 = Nothing}
validatePartialPrimInfo pPI@(PartialPrimInfo { primPart = p}) = Nothing

isPrimExpr :: Ctx -> Expr Var -> Maybe PrimInfo
isPrimExpr ctx expr = isPrimExpr' ctx expr >>= validatePartialPrimInfo

-- App (App (App (App f type) arg) Type2) arg2
isPrimExpr' :: Ctx -> Expr Var -> Maybe PartialPrimInfo
isPrimExpr' c (App e (Type t)) = case pPI of
  Just partPrimInfo ->
    case (argTypePart partPrimInfo, arg2TypePart partPrimInfo) of
    (Just _, Nothing) -> Just partPrimInfo {arg2TypePart = Just t}
    (Nothing, Nothing) -> Just partPrimInfo {argTypePart = Just t}
    _ -> Nothing
  Nothing -> Nothing
  where pPI = isPrimExpr' c e
isPrimExpr' c (App e e') =
  case isPrimExpr' c e of
    Just partPrimInfo@(PartialPrimInfo { primPart = Delay}) -> Just partPrimInfo {argVarPart = Just undefined}    -- UGLY HACK!!! Our data model does not suit delay well.
    Just partPrimInfo@(PartialPrimInfo { argVarPart = Nothing, arg2VarPart = Nothing}) -> Just partPrimInfo {argVarPart = getMaybeVar e'}
    Just partPrimInfo@(PartialPrimInfo { argVarPart = Just _, arg2VarPart = Nothing}) -> Just partPrimInfo {arg2VarPart = getMaybeVar e'}
    _ -> Nothing
isPrimExpr' c (Var v) = case isPrim c v of
  Just p ->  Just $ createPartialPrimInfo p v
  Nothing -> Nothing
isPrimExpr' c (Tick _ e) = isPrimExpr' c e
isPrimExpr' c (Lam v e)
  | isTyVar v || (not $ tcIsLiftedTypeKind $ typeKind $ varType v) = isPrimExpr' c e
isPrimExpr' _ _ = Nothing

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

data CheckResult = CheckResult{
  foundClock :: Maybe SymbolicClock
} deriving Show

advSelect :: CheckResult -> Bool
advSelect = isJust . foundClock

data CheckExpr = CheckExpr{
  recursiveSet :: Set Var,
  oldExpr :: Expr Var,
  fatalError :: Bool,
  verbose :: Bool,
  allowRecExp :: Bool
  }

checkExpr :: CheckExpr -> Expr Var -> CoreM CoreExpr
checkExpr c e = do
  --let check = countAdvSelect' (emptyCtx c) e
  e' <- transform (emptyCtx c) e
  --name <- retrieveName "Rattus.Plugin.Replacements" "adv'"
  --case name of
  --  _ -> putMsgS ("Case stmt" ++ showSDocUnsafe (ppr name))
  -- case check of
  --  Left s -> putMsgS s
  --  Right result -> putMsgS "Success"
  putMsgS "OLD-AST"
  putMsg (ppr e)
  putMsgS "NEW AST"
  putMsg (ppr e')
  putMsgS "NEW TREE SHOW"
  putMsgS (showTree e')
  return e'
{-
  --res <- checkExpr' (emptyCtx c) e
  case res of
    Nothing -> return ()
    Just (TypeError src doc) ->
      let sev = if fatalError c then SevError else SevWarning
      in if verbose c then do
        printMessage sev src ("Internal error in Rattus Plugin: single tick transformation did not preserve typing." $$ doc)
        liftIO $ putStrLn "-------- old --------"
        liftIO $ putStrLn (showSDocUnsafe (ppr (oldExpr c)))
        liftIO $ putStrLn "-------- new --------"
        liftIO $ putStrLn (showSDocUnsafe (ppr e))
         else
        printMessage sev noSrcSpan ("Internal error in Rattus Plugin: single tick transformation did not preserve typing." $$
                             "Compile with flags \"-fplugin-opt Rattus.Plugin:debug\" and \"-g2\" for detailed information")
-}
{-
checkExpr' :: Ctx -> Expr Var -> CoreM (Maybe TypeError)
checkExpr' c (App e e') | isType e' || (not $ tcIsLiftedTypeKind $ typeKind $ exprType e')
  = checkExpr' c e
checkExpr' c@Ctx{current = cur, hidden = hid, earlier = earl} (App e1 e2) =
  case isPrimExpr c e1 of
    Just (p,v) -> case p of
      Box -> do
        checkExpr' (stabilize BoxApp c) e2
      Arr -> do
        checkExpr' (stabilize BoxApp c) e2

      Delay -> (if inDelay c then typeError c v (text "Nested delays not allowed")
                else checkExpr' c{current = Set.empty, earlier = Just cur, inDelay = True} e2)
      Adv -> case earl of
        Just er -> if hasSeenAdvSelect c then typeError c v (text "Only one adv/select allowed in a delay")
                   else
                    D.trace ("ADV arg: " ++ showSDocUnsafe (ppr e1)) $
                    if isVar e2
                    then return Nothing
                    else typeError c v (text "can only advance on a variable")

        Nothing -> typeError c v (text "can only advance under delay")
      Select -> typeError c v (text "select not implemented")
        --case earl of
        --Just er | hasSeenAdvSelect c -> typeError c v (text "Only one adv/select allowed in a delay")
        --        | not $ all isVar args -> return Nothing
        --        | otherwise -> typeError c v (text "can only advance on a variable")
        --Nothing -> typeError c v (text "Can only select under delay")
    _ -> liftM2 (<|>) (checkExpr' c e1)  (checkExpr' c e2)
checkExpr' c (Case e v _ alts) =
    liftM2 (<|>) (checkExpr' c e) (liftM (foldl' (<|>) Nothing)
                                   (mapM ((\ (_,vs,e)-> checkExpr' (addVars vs c') e) . getAlt) alts))
  where c' = addVars [v] c
checkExpr' c (Lam v e)
  | isTyVar v || (not $ tcIsLiftedTypeKind $ typeKind $ varType v) = do
      is <- isStableConstr (varType v)
      let c' = case is of
            Nothing -> c
            Just t -> c{stableTypes = Set.insert t (stableTypes c)}
      checkExpr' c' e
  | otherwise = checkExpr' (addVars [v] (stabilizeLater c)) e
checkExpr' _ (Type _)  = return Nothing
checkExpr' _ (Lit _)  = return Nothing
checkExpr' _ (Coercion _)  = return Nothing
checkExpr' c (Tick (SourceNote span _name) e) =
  checkExpr' c{srcLoc = fromRealSrcSpan span} e
checkExpr' c (Tick _ e) = checkExpr' c e
checkExpr' c (Cast e _) = checkExpr' c e
checkExpr' c (Let (NonRec v e1) e2) =
  case isPrimExpr c e1 of
    Just (p,_) -> (checkExpr' (c{primAlias = Map.insert v p (primAlias c)}) e2)
    Nothing -> liftM2 (<|>) (checkExpr' c e1)  (checkExpr' (addVars [v] c) e2)
checkExpr' _ (Let (Rec ([])) _) = return Nothing
checkExpr' c (Let (Rec binds) e2) = do
    r1 <- mapM (\ (v,e) -> checkExpr' (c' v) e) binds
    r2 <- checkExpr' (addVars vs c) e2
    return (foldl' (<|>) Nothing r1 <|> r2)
  where vs = map fst binds
        ctxHid = maybe (current c) (Set.union (current c)) (earlier c)
        c' v = c {current = Set.empty,
                  earlier = Nothing,
                  hidden =  hidden c `Map.union`
                   (Map.fromSet (const (NestedRec v)) ctxHid),
                  recDef = recDef c `Set.union` Set.fromList vs }
checkExpr' c  (Var v)
  | tcIsLiftedTypeKind $ typeKind $ varType v =  case getScope c v of
             Hidden reason -> typeError c v reason
             Visible -> return Nothing
  | otherwise = return Nothing
-}


addVars :: [Var] -> Ctx -> Ctx
addVars v c = c{current = Set.fromList v `Set.union` current c }

emptyCheckResult :: CheckResult
emptyCheckResult = CheckResult {foundClock = Nothing}

updateCtxFromResult :: Ctx -> CheckResult -> Ctx
updateCtxFromResult c r = if advSelect r then c {hasSeenAdvSelect = advSelect r} else c

--checkAndUpdate :: Ctx -> Expr Var -> Either String Ctx
--checkAndUpdate c e = fmap (updateCtxFromResult c) (countAdvSelect' c e)

{-
-- called on the subtree to which a delay is applied
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

transformAdv :: Ctx -> Expr Var -> CoreM (Expr Var, (Var, Type))
transformAdv ctx expr@(App e e') = case isPrimExpr ctx expr of
  Just (PrimInfo {prim = Adv, function = f, arg = arg}) -> do
    varAdv' <- adv'Var
    let newE = replaceVar f varAdv' e
    return (App (App newE e') (Var (fromJust $ fresh ctx)), arg)
  _ -> do
        --fatalErrorMsgS "CANNOT TRANSFORM NON PRIMITIVES" 
        error "transformAdv: Can only transform adv"
transformAdv _ _ = do
  --fatalErrorMsgS "CANNOT TRANSFORM ANYTHING ELSE THAN PRIM EXPRESSIONS"
  error "CANNOT TRANSFORM ANYTHING ELSE THAN PRIM EXPRESSIONS"

transformSelect :: Ctx -> Expr Var -> CoreM ((Expr Var, (Var, Type), (Var, Type)))
transformSelect ctx expr@(App e e') = case isPrimExpr ctx expr of
  Just (PrimInfo {prim = Select, function = f, arg = arg, arg2 = Just arg2}) -> do
    varSelect' <- select'Var
    let newE = replaceVar f varSelect' e
    return ((App (App newE e') (Var (fromJust $ fresh ctx))), arg, arg2)
  _ -> do
        --fatalErrorMsgS "CANNOT TRANSFORM NON PRIMITIVES" 
        error "transformAdv: Can only transform select"
transformSelect _ _ = do
  --fatalErrorMsgS "CANNOT TRANSFORM ANYTHING ELSE THAN PRIM EXPRESSIONS"
  error "CANNOT TRANSFORM ANYTHING ELSE THAN PRIM EXPRESSIONS"

transform :: Ctx -> Expr Var -> CoreM (Expr Var)
transform ctx e = do
  (e', _, _) <- transform' ctx e
  return e'

hdd :: (a, b, c) -> a
hdd (a, b, c) = a


clockUnion :: Var -> Var -> Var -> (Var,Type) -> (Var, Type) -> Expr Var
clockUnion unionVar ordInt extractClock (arg1Var, arg1Type) (arg2Var, arg2Type) =
  App
  (
    App
    (
      App
      (
        App (Var unionVar) (Type intTy)
      )
      (
        Var ordInt
      )
    )
    (
      App
      (
        App (Var extractClock) (Type arg1Type)
      )
      (
        Var arg1Var
      )
    )
  )
  (
    App
    (
      App (Var extractClock) (Type arg2Type)
    )
    (
      Var arg2Var
    )
  )

transform' :: Ctx -> Expr Var -> CoreM ((Expr Var, Maybe (Var, Type), Maybe (Var, Type)))
transform' ctx expr@(App e e') = case isPrimExpr ctx expr of
  Just (PrimInfo {prim = Adv}) -> do
    (newExpr, arg) <- transformAdv ctx expr
    return $ (newExpr, Just (arg), Nothing)
  Just (PrimInfo {prim = Select}) -> do
    (newExpr, arg, arg2) <- transformSelect ctx expr
    return $ (newExpr, Just arg, Just arg2)
  Just (PrimInfo {prim = Delay, arg=(_, typ)}) -> do
    bigDelayVar <- bigDelay
    inputValueV <- inputValueVar
    extractClock <- extractClockVar
    let inputValueType = mkTyConTy inputValueV --Change name of variable
    inpVar <- mkSysLocalM (fsLit "inpV") inputValueType inputValueType
    let inpVarR = lazySetIdInfo inpVar vanillaIdInfo -- Unsure about this - we convert this to a real var with idInfo
    let ctx' = ctx {fresh = Just inpVarR}
    (newExpr, arg, arg2) <- transform' ctx' e'
    case (arg, arg2) of
      (Just (clVar, typeAdv), Nothing) -> do
        let lambdaExpr = Lam inpVarR newExpr
        return $ ((App (App (Var bigDelayVar) (App (App (Var extractClock) (Type (typeAdv))) (Var clVar))) lambdaExpr), Nothing, Nothing) --App e e'
      (Just (clVar, typeAdv), Just (clVar2, typeAdv2)) -> do
        let lambdaExpr = Lam inpVarR newExpr
        ordInt <- ordIntClass
        unionV <- unionVar
        --lambdaVar <- fail "hello"
        return $ ((App (App (Var bigDelayVar) (clockUnion unionV ordInt extractClock (clVar, typeAdv) (clVar2, typeAdv2))) lambdaExpr), Nothing, Nothing) --App e e' (clockUnion unionV ordInt extractClock (clVar, typeAdv) (clVar2, typeAdv2))
      (Nothing, Nothing) -> error "NO CLOCK PRESENT"
      (_,_) -> error "INCORRECT STRUCTURE (CANNOT HAPPEN I THINK)"
  Just _ -> do
        (newExpr, cl, typeAdv) <- transform' ctx e'
        return $ (App e newExpr, cl, typeAdv)
  Nothing -> do
    (expr, cl, typeAdv) <- transform' ctx e
    (expr', cl', typeAdv') <- transform' ctx e'
    case (cl, cl') of
      (Just c1, Just c2) -> error "MULTIPLE CLOCKS"
      (Just c1, Nothing) -> return (App expr expr', Just c1, typeAdv)
      (Nothing, Just c2) -> return (App expr expr', Just c2, typeAdv')
      (Nothing, Nothing) -> return (App expr expr', Nothing, Nothing)
transform' ctx (Lam b rhs) = transform' ctx rhs >>= \(expr, cl, typeAdv) -> return (Lam b expr, cl, typeAdv)
transform' ctx (Let (NonRec b e') e) = do
    (nonRecExpr, cl, typeAdv) <- transform' ctx e'
    (letBodyExpr, cl',typeAdv') <- transform' ctx e
    case (cl, cl') of
      (Just c1, Just c2) -> error "MULTIPLE CLOCKS"
      (Just c1, Nothing) -> return (Let (NonRec b nonRecExpr) letBodyExpr, Just c1, typeAdv)
      (Nothing, Just c2) -> return (Let (NonRec b nonRecExpr) letBodyExpr, Just c2, typeAdv')
      (Nothing, Nothing) -> return (Let (NonRec b nonRecExpr) letBodyExpr, Nothing, Nothing)
transform' ctx (Case e b t alts) = do
    (expr, cl, typeAdv) <- transform' ctx e
    alts' <- mapM (\(Alt con binds expr) -> transform' ctx expr <&> (Alt con binds . hdd)) alts
    return (Case expr b t alts', cl, typeAdv)
transform' ctx (Cast e _) = transform' ctx e
transform' ctx (Tick _ e) = transform' ctx e
transform' _ e = return (e, Nothing, Nothing)