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
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Applicative
import Data.Foldable
import Data.Maybe (isJust, fromJust)
import qualified Debug.Trace as D
import GHC.Types.Name.Cache (NameCache(nsNames), lookupOrigNameCache, OrigNameCache)
import qualified GHC.Types.Name.Occurrence as Occurrence
import Data.IORef (readIORef)
import GHC.Unit.External (ExternalPackageState (eps_PTE))
import GHC.Types.TyThing (tyThingId)
import GHC.Types.TypeEnv
import GHC.Unit.Home.ModInfo
import GHC.Unit.Module.ModDetails

type LCtx = Set Var
data HiddenReason = BoxApp | AdvApp | NestedRec Var | FunDef | DelayApp
type Hidden = Map Var HiddenReason

data Prim = Delay | Adv | Box | Arr | Select

data TypeError = TypeError SrcSpan SDoc

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
    hasSeenAdvSelect :: Bool
    }

hasTick :: Ctx -> Bool
hasTick = isJust . earlier

origNameCache :: CoreM OrigNameCache
origNameCache = do
  hscEnv <- getHscEnv
  nameCache <- liftIO $ readIORef (hsc_NC hscEnv)
  return $ nsNames nameCache

externalPackageState :: CoreM ExternalPackageState
externalPackageState = do
  hscEnv <- getHscEnv
  liftIO $ readIORef $ hsc_EPS hscEnv

homePackageState :: CoreM HomePackageTable
homePackageState = do
  hsc_HPT <$> getHscEnv

homeModInfo :: Module -> CoreM (Maybe HomeModInfo)
homeModInfo mod = do
  hpt <- homePackageState
  return $ lookupHptByModule hpt mod

getHomeTyEnv :: Module -> CoreM (Maybe TypeEnv)
getHomeTyEnv mod = do
  maybeHmi <- homeModInfo mod
  let details = fmap hm_details maybeHmi
  return $ fmap md_types details

getExtTyEnv :: CoreM TypeEnv
getExtTyEnv = do
  eps_PTE <$> externalPackageState

nameToVar :: Module -> Name -> CoreM (Maybe Id)
nameToVar mod n = do
  putMsg $ text "nameToVar, name: " <> ppr n <> text " | mod: " <> ppr mod
  extTyEnv <- getExtTyEnv
  maybeHomeTyEnv <- getHomeTyEnv mod

  case maybeHomeTyEnv of
    Just homeTyEnv -> return (tyThingId <$> lookupNameEnv homeTyEnv n)
    Nothing -> return (tyThingId <$> lookupNameEnv extTyEnv n)

findName :: OccName -> Module -> CoreM Name
findName occName mod = do
  putMsg $ text "findName " <> ppr occName <> text " | " <> ppr mod
  onc <- origNameCache
  return $ fromJust $ lookupOrigNameCache onc mod occName

adv'Var :: CoreM Var
adv'Var = do
  rattusMods <- rattusModules
  let [mod] = (filter (("Rattus.Primitives" ==) . unpackFS . getModuleFS) . moduleEnvKeys) rattusMods
  putMsg $ text "adv'Var: mod " <> ppr mod
  let occName = mkOccName Occurrence.varName "adv'"
  name <- findName occName mod
  maybeId <- nameToVar mod name
  return $ fromJust maybeId

rattusModules :: CoreM (ModuleEnv (OccEnv Name))
rattusModules = do
  origNameCache <- origNameCache
  let rattusBindings = filterModuleEnv (\mod _ -> isRattModule (getModuleFS mod)) origNameCache
  return rattusBindings

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
        hasSeenAdvSelect = False
        }


showMaybePrimExpr:: Maybe (Prim, Var, [Expr Var]) -> SDoc
showMaybePrimExpr mpe = case mpe of
  Just (p, v, le) -> (text "Just (") <> ppr p <> (text ") (") <> ppr v <> (text ") ([") <> foldl (\acc e -> acc <> ppr e <> text ", ") (text "") le
  Nothing -> text "Nothing"

isPrimExpr :: Ctx -> Expr Var -> Maybe (Prim, Var, [Expr Var])
isPrimExpr c (App (App f arg1) arg2) =
  case let r = (isPrimExpr c f) in D.trace ("Double-App: " ++ showSDocUnsafe (showMaybePrimExpr r <> text " | " <> ppr f)) r of
    Just (Select, v, []) -> D.trace "Yep, it's a select" (Just (Select, v, [arg1, arg2]))
    x -> x
isPrimExpr c (App e (Type _)) = isPrimExpr c e
isPrimExpr c (App e e') | not $ tcIsLiftedTypeKind $ typeKind $ exprType e' = case isPrimExpr c e of
  Just (p, v, []) -> Just (p, v, [e'])
  x -> x
isPrimExpr c (Var v) = fmap (,v, []) (isPrim c v)
isPrimExpr c (Tick _ e) = isPrimExpr c e
isPrimExpr c (Lam v e)
  | isTyVar v || (not $ tcIsLiftedTypeKind $ typeKind $ varType v) = isPrimExpr c e
isPrimExpr _ _ = Nothing


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

checkExpr :: CheckExpr -> Expr Var -> CoreM ()
checkExpr c e = do
  let check = countAdvSelect' (emptyCtx c) e
  av <- adv'Var
  putMsg $ text "checkExpr, adv' Var:" <> ppr av
  e' <- transform (emptyCtx c) e
  --name <- retrieveName "Rattus.Plugin.Replacements" "adv'"
  --case name of
  --  _ -> putMsgS ("Case stmt" ++ showSDocUnsafe (ppr name))
  case check of
    Left s -> putMsgS s
    Right result -> putMsgS "Success"
  putMsgS "NEW AST"
  putMsg (ppr e')
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
checkExpr' :: Ctx -> Expr Var -> CoreM (Maybe TypeError)
checkExpr' c (App e e') | isType e' || (not $ tcIsLiftedTypeKind $ typeKind $ exprType e')
  = checkExpr' c e
checkExpr' c@Ctx{current = cur, hidden = hid, earlier = earl} (App e1 e2) =
  case isPrimExpr c e1 of
    Just (p,v, args) -> case p of
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
      Select -> case earl of
        Just er | hasSeenAdvSelect c -> typeError c v (text "Only one adv/select allowed in a delay")
                | not $ all isVar args -> return Nothing
                | otherwise -> typeError c v (text "can only advance on a variable")
        Nothing -> typeError c v (text "Can only select under delay")
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
    Just (p,_, _) -> (checkExpr' (c{primAlias = Map.insert v p (primAlias c)}) e2)
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



addVars :: [Var] -> Ctx -> Ctx
addVars v c = c{current = Set.fromList v `Set.union` current c }

emptyCheckResult :: CheckResult
emptyCheckResult = CheckResult {foundClock = Nothing}

updateCtxFromResult :: Ctx -> CheckResult -> Ctx
updateCtxFromResult c r = if advSelect r then c {hasSeenAdvSelect = advSelect r} else c

checkAndUpdate :: Ctx -> Expr Var -> Either String Ctx
checkAndUpdate c e = fmap (updateCtxFromResult c) (countAdvSelect' c e)

-- called on the subtree to which a delay is applied
countAdvSelect' :: Ctx -> Expr Var -> Either String CheckResult
countAdvSelect' ctx (App e e') = case isPrimExpr ctx e of
      Just (p, _, args) -> case D.trace ("we have met a prim: " ++ showSDocUnsafe (ppr p)) p of
        Adv | not (isVar e') -> Left "Can only adv on variables"
            | hasSeenAdvSelect ctx -> Left "Only one adv/select allowed in a delay"
            | otherwise -> Right CheckResult { foundClock = Just (Clock (getVar e')) }
        Select | not $ all isVar args -> Left "Can only select on variables"
                    | hasSeenAdvSelect ctx -> Left "Only one adv/select allowed in a delay"
                    | otherwise -> Right CheckResult { foundClock = let [first, second] = map (Clock . getVar) args in Just $ Union first second}
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


transformAdv :: Ctx -> Expr Var -> CoreM (Maybe (Expr Var))
transformAdv ctx (App e _) = case isPrimExpr ctx e of
  Just (p, _, [arg]) -> case p of 
    Adv -> do
      putMsgS "I HIT TRANSFORM"
      varAdv' <- adv'Var
      return (Just (App (Var varAdv') arg))
    _ -> do
        fatalErrorMsgS "CANNOT TRANSFORM OTHER PRIMITIVES THAN ADV/SELECT" 
        return Nothing
  _ -> do
        fatalErrorMsgS "CANNOT TRANSFORM NON PRIMITIVES" 
        return Nothing
transformAdv _ _ = do 
  fatalErrorMsgS "CANNOT TRANSFORM ANYTHING ELSE THAN PRIM EXPRESSIONS"
  return Nothing

transform :: Ctx -> Expr Var -> CoreM (Expr Var)
transform ctx expr@(App e e') = case D.trace ("This is our application " ++ (showSDocUnsafe $ ppr e)) isPrimExpr ctx e of 
  Just (p, var, [arg]) -> case p of 
    Adv -> do
        putMsg $ text "Adv Expr before - " <> ppr e
        newExpr <- transformAdv ctx expr
        putMsg $ text "Adv Expr after - " <> ppr newExpr
        return $ fromJust newExpr
    _ -> transform ctx e'
  _ -> do
    expr <- transform ctx e
    expr' <- transform ctx e'
    return $ App expr expr'
transform ctx (Lam b rhs) = transform ctx rhs >>= (return . Lam b)  
transform ctx (Let (NonRec b e') e) = do 
    nonRecExpr <- transform ctx e'
    letBodyExpr <- transform ctx e
    return $ Let (NonRec b nonRecExpr) letBodyExpr
transform ctx (Case e b t alts) = do
    expr <- transform ctx e
    alts' <- mapM (\(Alt con binds expr) -> transform ctx expr >>= (return . Alt con binds)) alts
    return $ Case expr b t alts'
transform ctx (Cast e _) = transform ctx e
transform ctx (Tick _ e) = transform ctx e
transform _ e = return e