{-# LANGUAGE OverloadedStrings #-}

module Rattus.Plugin.PrimExpr (
    Prim (..),
    PrimInfo (..),
    isPrimExpr
) where

import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Plugins
import Rattus.Plugin.Utils
import Prelude hiding ((<>))

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

instance Outputable PartialPrimInfo where
  ppr (PartialPrimInfo prim f argT argV arg2T arg2V) = text "PartialPrimInfo { prim = " <> ppr prim <> text ", function = " <> ppr f <> text ", argT = " <> ppr argT <> text ", argV = " <> ppr argV <> text ", arg2T = " <> ppr arg2T <> ", arg2V = " <> ppr arg2V

instance Outputable Prim where
  ppr Delay = "delay"
  ppr Adv = "adv"
  ppr Select = "select"
  ppr Box = "box"
  ppr Arr = "arr"
  
primMap :: Map FastString Prim
primMap = Map.fromList
  [("delay", Delay),
   ("adv", Adv),
   ("select", Select),
   ("box", Box),
   ("arr", Arr)
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
    argTypePart = Nothing,
    argVarPart = Nothing,
    arg2TypePart = Nothing,
    arg2VarPart = Nothing
  }


validatePartialPrimInfo :: PartialPrimInfo -> Maybe PrimInfo
validatePartialPrimInfo (PartialPrimInfo Select f (Just argT) (Just argV) (Just arg2T) (Just arg2V)) = Just PrimInfo { prim = Select, function = f, arg = (argV, argT), arg2 = Just (arg2V, arg2T)}
validatePartialPrimInfo (PartialPrimInfo {primPart = Delay, functionPart = f, argTypePart = Just typ}) = Just PrimInfo {prim = Delay, function = f, arg = (undefined, typ), arg2 = Nothing}    -- UGLY HACK (connected to the one below)
validatePartialPrimInfo (PartialPrimInfo p f (Just argT) (Just argV) Nothing Nothing) = Just PrimInfo { prim = p, function = f, arg = (argV, argT), arg2 = Nothing}
validatePartialPrimInfo _ = Nothing

isPrimExpr :: Expr Var -> Maybe PrimInfo
isPrimExpr expr = isPrimExpr' expr >>= validatePartialPrimInfo 

-- App (App (App (App f type) arg) Type2) arg2
isPrimExpr' :: Expr Var -> Maybe PartialPrimInfo
isPrimExpr' (App e (Type t)) = case pPI of
  Just partPrimInfo ->
    case (argTypePart partPrimInfo, arg2TypePart partPrimInfo) of
    (Just _, Nothing) -> Just partPrimInfo {arg2TypePart = Just t}
    (Nothing, Nothing) -> Just partPrimInfo {argTypePart = Just t}
    _ -> Nothing
  Nothing -> Nothing
  where pPI = isPrimExpr' e
isPrimExpr' (App e e') =
  case isPrimExpr' e of
    Just partPrimInfo@(PartialPrimInfo { primPart = Delay}) -> Just partPrimInfo {argVarPart = Just undefined}    -- UGLY HACK!!! Our data model does not suit delay well.
    Just partPrimInfo@(PartialPrimInfo { argVarPart = Nothing, arg2VarPart = Nothing}) -> Just partPrimInfo {argVarPart = getMaybeVar e'}
    Just partPrimInfo@(PartialPrimInfo { argVarPart = Just _, arg2VarPart = Nothing}) -> Just partPrimInfo {arg2VarPart = getMaybeVar e'}
    _ -> Nothing
isPrimExpr' (Var v) = case isPrim v of
  Just p ->  Just $ createPartialPrimInfo p v
  Nothing -> Nothing
isPrimExpr' (Tick _ e) = isPrimExpr' e
isPrimExpr' (Lam v e)
  | isTyVar v || (not $ tcIsLiftedTypeKind $ typeKind $ varType v) = isPrimExpr' e
isPrimExpr' _ = Nothing
