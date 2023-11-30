{-# LANGUAGE OverloadedStrings #-}

module AsyncRattus.Plugin.PrimExpr (
    Prim (..),
    PrimInfo (..),
    function,
    prim,
    isPrimExpr
) where

import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Plugins
import AsyncRattus.Plugin.Utils
import Prelude hiding ((<>))

data Prim = Delay | Adv | Progress | Box | Select

-- DelayApp has the following fields: Var = delay f, T1 = value type, T2 = later type (O v a)
-- AdvApp has the following fields: Var = adv f, TypedArg = var and type for arg
data PrimInfo = DelayApp Var Type | AdvApp Var TypedArg | ProgressApp Var | BoxApp Var | SelectApp Var TypedArg TypedArg

type TypedArg = (Var, Type)

data PartialPrimInfo = PartialPrimInfo {
  primPart :: Prim,
  functionPart :: Var,
  args :: [Var],
  typeArgs :: [Type]
}

instance Outputable PartialPrimInfo where
  ppr (PartialPrimInfo Delay f _ typeArgs) = text "PartialPrimInfo { prim = Delay, function = " <> ppr f <> text "args = (not printing since it should be undefined) , typeArgs = " <> ppr typeArgs 
  ppr (PartialPrimInfo prim f args typeArgs) = text "PartialPrimInfo { prim = " <> ppr prim <> text ", function = " <> ppr f <> text ", args = " <> ppr args <> text ", typeArgs = " <> ppr typeArgs

instance Outputable Prim where
  ppr Delay = "delay"
  ppr Adv = "adv"
  ppr Progress = "progress"
  ppr Select = "select"
  ppr Box = "box"

instance Outputable PrimInfo where
  ppr (DelayApp f _) = text "DelayApp - function " <> ppr f 
  ppr (BoxApp f) = text "BoxApp - function " <> ppr f
  ppr (AdvApp f arg) = text "AdvApp - function " <> ppr f <> text " | arg " <> ppr arg
  ppr (ProgressApp f) = text "ProgressApp - function " <> ppr f
  ppr (SelectApp f arg arg2) = text "SelectApp - function " <> ppr f <> text " | arg " <> ppr arg <> text " | arg2 " <> ppr arg2
  
primMap :: Map FastString Prim
primMap = Map.fromList
  [("delay", Delay),
   ("adv", Adv),
   ("progress", Progress),
   ("select", Select),
   ("box", Box)
   ]


isPrim :: Var -> Maybe Prim
isPrim v = case getNameModule v of
    Just (name, mod) | isRattModule mod -> Map.lookup name primMap
    _ -> Nothing

createPartialPrimInfo :: Prim -> Var -> PartialPrimInfo
createPartialPrimInfo prim function =
  PartialPrimInfo {
    primPart = prim,
    functionPart = function,
    args = [],
    typeArgs = []
  }

function :: PrimInfo -> Var
function (DelayApp f _) = f
function (BoxApp f) = f
function (AdvApp f _) = f
function (ProgressApp f) = f
function (SelectApp f _ _) = f

prim :: PrimInfo -> Prim
prim (DelayApp {}) = Delay
prim (BoxApp _) = Box
prim (AdvApp {}) = Adv
prim (ProgressApp {}) = Progress
prim (SelectApp {}) = Select

validatePartialPrimInfo :: PartialPrimInfo -> Maybe PrimInfo
validatePartialPrimInfo (PartialPrimInfo Select f [arg2V, argV] [arg2T, argT]) = Just $ SelectApp f (argV, argT) (arg2V, arg2T)
validatePartialPrimInfo (PartialPrimInfo Delay f [_] [argT]) = Just $ DelayApp f argT
validatePartialPrimInfo (PartialPrimInfo {primPart = Box, functionPart = f}) = Just $ BoxApp f
validatePartialPrimInfo (PartialPrimInfo Adv f [argV] [argT]) = Just $ AdvApp f (argV, argT)
validatePartialPrimInfo _ = Nothing

isPrimExpr :: Expr Var -> Maybe PrimInfo
isPrimExpr expr = isPrimExpr' expr >>= validatePartialPrimInfo

isPrimExpr' :: Expr Var -> Maybe PartialPrimInfo
isPrimExpr' (App e (Type t)) = case mPPI of
  Just pPI@(PartialPrimInfo {typeArgs = tArgs}) -> Just pPI {typeArgs = t : tArgs}
  Nothing -> Nothing
  where mPPI = isPrimExpr' e
isPrimExpr' (App e e') =
  case isPrimExpr' e of
    Just partPrimInfo@(PartialPrimInfo { primPart = Delay, args = args}) -> Just partPrimInfo {args = undefined : args}
    Just partPrimInfo@(PartialPrimInfo { args = args}) -> Just partPrimInfo {args = maybe args (:args) (getMaybeVar e')}
    _ -> Nothing
isPrimExpr' (Var v) = case isPrim v of
  Just p ->  Just $ createPartialPrimInfo p v
  Nothing -> Nothing
isPrimExpr' (Tick _ e) = isPrimExpr' e
isPrimExpr' (Lam v e)
  | isTyVar v || (not $ tcIsLiftedTypeKind $ typeKind $ varType v) = isPrimExpr' e
isPrimExpr' _ = Nothing
