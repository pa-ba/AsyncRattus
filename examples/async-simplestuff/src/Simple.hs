module Simple where

import Rattus
import Rattus.Stream (Str(..))
import Rattus.ToHaskell
import Rattus.Primitives
import qualified Data.Set as Set

{-# ANN module Rattus #-}

mkChannel :: Int -> (InputValue -> a) -> O a
mkChannel id = Delay (Set.singleton id)

keyboard :: O Char
keyboard = mkChannel 1 (\(1, CharValue c) -> c)

reset :: O Bool
reset = mkChannel 2 (\(2, BoolValue b) -> b)


--------------------------------------

-- Streams based on input channels
kbStr :: O (Str Char)
kbStr = delay (adv keyboard ::: delay (adv kbStr))
