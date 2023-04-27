{-# LANGUAGE TypeOperators #-}

module Sheet where 

import Rattus (Rattus(..))
import qualified Rattus.Channels as Channels
import Rattus.Channels (mkChannels)
import qualified Rattus.Primitives as Prim
import Rattus.Primitives (box, unbox, select, delay, adv)
import qualified Rattus.Stream as Stream
import Rattus.Stream(Str(..))
import qualified Rattus.Later as Later
import qualified Rattus.Strict as Strict
import Rattus.Strict (List(..), (+++), (:*))
import qualified Data.Set
import qualified Data.Map as Map
import Data.Map ((!))

import Expr

type Cell = String
data CellUpdate = NewFormula Expr | UpdatedDependency (Cell, Maybe Int)

type Input = Int
type O a = Prim.O Input a
type Stream a = Str Input a
type InputChannel = Channels.InputChannel Input

{-# ANN module Rattus #-}

-- Identifier, value of cell, dependent on these cells
--type Cell f = (String, f, O Int)
--type Spreadsheet = Map String Cell

inputCells = ["A1", "A2", "B1", "B2"]

(input, inputMaybe, depend, channels) = mkChannels inputCells

([a1, a2, b1, b2]) = map Stream.fromLater channels

c1 :: Stream Int
c1 = Stream.zipWithAwait (box (+)) a1 b1 0 0

a3 :: Stream Int
a3 = Stream.zipWithAwait (box (+)) a1 a2 0 0

miniSheet :: Stream (Int :* Int)
miniSheet = Stream.zip c1 a3

--------------------------------------------------------------------

{-
cellToChannel = Map.fromList $ zip cellStrings channels
channelToCell = Map.fromList $ zip channels cellStrings

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


spreadSheet :: Stream (Map Cell (Maybe Int))


spreadSheet' :: Stream (Map Cell (InputChannel, Expr)) -> Stream (Map Cell (Expr, Maybe Int))
spreadSheet' (updates ::: updateStr) = undefined
    where 
        lAssocs = Strict.map (\(ch, cell) -> Later.map (cell,) ch) (Map.assocs channelToCell)
        -- O (List (Int, (cell, expr)))
        lUpdatedChannels = Later.selectMany lAssocs
        -- O (List (cellId, expr))
        lUpdatedChannels' = Later.map (Strict.map snd) lUpdatedChannels




cell :: Expr -> Stream VarEnv -> Stream (Maybe Int)
cell e = Stream.map (evalMaybe e)

-- assume we know this expr has no dependencies
constCell :: Expr -> Stream (Maybe Int)
constCell = Stream.const . flip evalMaybe (emptyVarEnv)

-- assume this expr has a single dependency
singleDependencyCell :: Expr -> O (Stream (Maybe Int)) -> Stream (Maybe Int)
singleDependencyCell e dependency = Stream.map (evalMaybe e) varEnvStr
    where varEnvStr = varEnv (Strict.singleton dependency) emptyVarEnv

-- assume this expr has a single dependency
cell :: Expr -> List (O (Stream (Maybe Int))) -> Stream (Maybe Int)
cell e dependencies = Stream.map (evalMaybe e) varEnvStr
    where varEnvStr = varEnv dependencies emptyVarEnv

mkCell :: Expr -> Map Cell (O (Stream (Maybe Int))) -> Stream (Maybe Int)
mkCell e cellStrs = cell e dependencyStrs
    where dependencies = depends e
          dependencyStrs = Map.foldWithKey (\cellId cellStr acc -> if cellId `elem` dependencies then cellStr :! acc else acc) Nil cellStrs

resettableCell :: Stream VarEnv -> Stream Expr -> Stream (Maybe Int)
resettableCell envStr exprStr = 
    Stream.map (\e -> cell e envStr) exprStr
    
    cell expr envStr ::: delay (
        case select envs exprs of
            Left envStr lExprStr -> resettableCell env (expr ::: lExprStr)
            Right lEnvStr exprStr' -> resettableCell (env ::: lEnvStr) exprStr'
            Both envStr exprStr' -> resettableCell envStr exprStr'
    )
-}