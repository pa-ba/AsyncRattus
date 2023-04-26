module Sheet where 

import Rattus (Rattus(..))
import Rattus.Channels
import qualified Rattus.Primitives as Prim
import Rattus.Primitives (box, unbox, select, delay, adv)
import qualified Rattus.Stream as Stream
import Rattus.Stream(Str(..))
import qualified Rattus.Later as Later
import qualified Rattus.Strict as Strict
import Rattus.Strict (List(..), (+++))
import qualified Data.Set
import qualified Data.Map as Map
import Data.Map ((!))

import Expr

type Cell = String
data CellUpdate = NewFormula Expr | UpdatedDependency (Cell, Maybe Int)

type Input = (Cell, Expr)
type O a = Prim.O Input a
type Stream a = Str Input a

{-# ANN module Rattus #-}

-- Identifier, value of cell, dependent on these cells
--type Cell f = (String, f, O Int)
--type Spreadsheet = Map String Cell

cells = [(x, y) | x <- ['A' .. 'E'], y <- [1 .. 5]]

-- Produces strings "A1", "A2", ... "B1", ... "E5"
cellStrings = map (\(x,y) -> [x] ++ show y) cells

(input, inputMaybe, depend, channels) = mkChannels cellStrings

cellToChannel = Map.fromList $ zip cellStrings channels

updateVarEnv :: List (Cell, Maybe Int) -> VarEnv -> VarEnv
updateVarEnv lst env = foldl folder env lst
    where
        folder acc (c, Just i) = Map.insert c i acc
        folder acc (c, Nothing) = acc

{-# ANN removeIndices AllowRecursion #-}
removeIndices :: List Int -> List a -> List a
removeIndices = aux 0
    where 
        aux i Nil lst = lst
        aux i indices Nil = Nil
        aux i indices@(index :! indices') (x :! xs)
            | i == index = aux (i+1) indices' xs
            | otherwise  = x :! (aux (i+1) indices xs)

-- Given a list of cell streams, produces as stream of corresponding variable environments
varEnv :: List (O (Stream (Cell, Maybe Int))) -> VarEnv -> O (Stream VarEnv)
varEnv delayedStreams prevEnv = delay (
        let updatedStreams = adv (Later.selectMany delayedStreams)
            updatedIndices = Strict.map' fst updatedStreams
            staleStreams = removeIndices updatedIndices delayedStreams
            strs = Strict.map' snd updatedStreams
            updates = Strict.map' (Stream.hd) strs
            newEnv = updateVarEnv updates prevEnv
            newStrs = Strict.map' (Stream.tl) strs
        in newEnv ::: varEnv (staleStreams +++ newStrs) newEnv
    )
    where
        fold acc (cell, value) = Map.insert cell value acc

testVarEnv :: O (Stream VarEnv)
testVarEnv = varEnv (Strict.map' (Later.map (Stream.map (box (\(c, e) -> (c, evalMaybe e emptyVarEnv))))) streams) emptyVarEnv
    where
        cell s = cellToChannel ! s
        cellStr s = Stream.fromLater (cell s)
        streams = (cellStr "A3" :! cellStr "A2" :! cellStr "A1" :! Nil)
        evalMaybe' (Just e) = evalMaybe e emptyVarEnv
        evalMaybe' Nothing = Nothing


cell :: Expr -> O (Stream VarEnv) -> O (Stream (Maybe Int))
cell e envs = Later.map (Stream.map (box (evalMaybe e))) envs

{-
-- 1st arg: initial variable environment
-- 2nd arg: Stream of cell updates
resettableCell :: O (Stream VarEnv) -> O (Stream CellUpdate) -> O (Stream (Maybe Int))
resettableCell varEnvs updates = delay (
        case adv updates of
            NewFormula e ::: us -> undefined
            UpdatedDependency (cellId, maybeValue) ::: us -> undefined
    )
-}