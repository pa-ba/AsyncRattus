module Sheet where 

import Rattus.Channels
import qualified Rattus.Primitives as Prim
import Rattus.Primitives (box, unbox, select, delay, adv)
import qualified Rattus.Stream as Stream
import qualified Rattus.Later as Later
import qualified Rattus.Strict as Strict
import qualified Data.Set
import qualified Data.Map as Map

import Expr

type Cell = String
data CellUpdate = NewFormula Expr | UpdatedDependency (Cell, Maybe Int)

type Input = CellFormula
type O a = Prim.O Input a
type Str a = Stream.Str Input a

-- Identifier, value of cell, dependent on these cells
--type Cell f = (String, f, O Int)
--type Spreadsheet = Map String Cell

cells = [(x, y) | x <- ['A' .. 'E'], y <- [1 .. 5]]

-- Produces strings "A1", "A2", ... "B1", ... "E5"
cellStrings = map (\(x,y) -> [x] ++ show y) cells

(input, inputMaybe, depend, channels) = mkChannels cellStrings

cellToChannel = Map.fromList $ zip cellStrings channels

-- Given a list of cell streams, produces as stream of corresponding variable environments
varEnv :: Strict.List (O (Str (Cell, Int))) -> O (Str VarEnv)
varEnv delayedStreams = scanAwait (box fold) emptyVarEnv delayedStreams
    where
        laterChangedCells = Later.selectMany delayedStreams
        strChangedCells = Stream.fromLater laterChangedCells
        fold acc (cell, value) = Map.insert cell value acc

cell :: Expr -> O (Str VarEnv) -> O (Str (Maybe Int))
cell e envs = Later.map (Stream.map (evalMaybe e)) envs

-- 1st arg: initial variable environment
-- 2nd arg: Stream of cell updates
resettableCell :: O (Str VarEnv) -> O (Str CellUpdate) -> O (Str (Maybe Int))
resettableCell varEnvs updates = delay (
        case adv updates of
            NewFormula e ::: us -> 
            UpdatedDependency (cellId, maybeValue) ::: us ->
    )

