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

type Cell = String

type Input = Int
type O a = Prim.O Input a
type Stream a = Str Input a
type InputChannel = Channels.InputChannel Input

{-# ANN module Rattus #-}

inputCells = ["A1", "A2", "B1", "B2"]

(input, inputMaybe, depend, channels) = mkChannels inputCells

([a1, a2, b1, b2]) = map Stream.fromLater channels

c1 :: Stream Int
c1 = Stream.zipWithAwait (box (+)) a1 b1 0 0

a3 :: Stream Int
a3 = Stream.zipWithAwait (box (+)) a1 a2 0 0

miniSheet :: Stream (Int :* Int)
miniSheet = Stream.zip c1 a3
