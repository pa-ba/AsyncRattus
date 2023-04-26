module Sheet where 

import Rattus.Channels
import qualified Rattus.Primitives as Prim
import Rattus.Primitives (box, unbox, select, delay, adv)
import qualified Rattus.Stream as Stream
import Rattus.Stream(Str(..))
import qualified Rattus.Later as Later
import qualified Rattus.Strict as Strict
import qualified Data.Set
import qualified Data.Map as Map

import Expr

type Cell = String
data CellUpdate = NewFormula Expr | UpdatedDependency (Cell, Maybe Int)

type Input = (Cell, Expr)
type O a = Prim.O Input a
type Stream a = Str Input a

-- Identifier, value of cell, dependent on these cells
--type Cell f = (String, f, O Int)
--type Spreadsheet = Map String Cell

cells = [(x, y) | x <- ['A' .. 'E'], y <- [1 .. 5]]

-- Produces strings "A1", "A2", ... "B1", ... "E5"
cellStrings = map (\(x,y) -> [x] ++ show y) cells

(input, inputMaybe, depend, channels) = mkChannels cellStrings

cellToChannel = Map.fromList $ zip cellStrings channels

-- Given a list of cell streams, produces as stream of corresponding variable environments
{-varEnv :: Strict.List (O (Stream (Cell, Int))) -> Stream VarEnv
varEnv delayedStreams = Stream.scanAwait (box fold) emptyVarEnv strChangedCells
    where
        laterChangedCells = Later.selectMany delayedStreams
        strChangedCells = Stream.fromLater laterChangedCells
        fold acc (cell, value) = Map.insert cell value acc
-}
cell :: Expr -> O (Stream VarEnv) -> O (Stream (Maybe Int))
cell e envs = Later.map (Stream.map (box (evalMaybe e))) envs

-- 1st arg: initial variable environment
-- 2nd arg: Stream of cell updates
resettableCell :: O (Stream VarEnv) -> O (Stream CellUpdate) -> O (Stream (Maybe Int))
resettableCell varEnvs updates = delay (
        case adv updates of
            NewFormula e ::: us -> undefined
            UpdatedDependency (cellId, maybeValue) ::: us -> undefined
    )

