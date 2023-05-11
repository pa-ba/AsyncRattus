{-# LANGUAGE TypeOperators #-}

module Sheet where 

import AsyncRattus (AsyncRattus(..))
import qualified AsyncRattus.Channels as Channels
import AsyncRattus.Channels (mkChannels)
import qualified AsyncRattus.Primitives as Prim
import AsyncRattus.Primitives (box, unbox, select, delay, adv)
import qualified AsyncRattus.Stream as Stream
import AsyncRattus.Stream(Str(..))
import qualified AsyncRattus.Later as Later
import qualified AsyncRattus.Strict as Strict
import AsyncRattus.Strict (List(..), (+++), (:*))
import qualified Data.Set
import qualified Data.Map as Map
import Data.Map ((!))

type Cell = String

type Input = Int
type O a = Prim.O Input a
type Stream a = Str Input a
type InputChannel = Channels.InputChannel Input

{-# ANN module AsyncRattus #-}

inputCells = Strict.fromList ["A1", "A2", "B1", "B2"]

(input, inputMaybe, depend, channels) = mkChannels inputCells

(a1 :! a2 :! b1 :! b2 :! Nil) = Strict.map' (Stream.fromLater) channels

c1 :: Stream Int
c1 = Stream.zipWithAwait (box (+)) a1 b1 0 0

a3 :: Stream Int
a3 = Stream.zipWithAwait (box (+)) a1 a2 0 0

miniSheet :: Stream (Int :* Int)
miniSheet = Stream.zip c1 a3
