{-# OPTIONS -fplugin=Rattus.Plugin #-}

module Rattus.Later (
    map
) where

import Prelude hiding (map)
import Rattus
import Rattus.Primitives ( O, delay, adv )

{-# ANN module Rattus #-}

map :: (a -> b) -> O v a -> O v b
map f later = delay (f (adv later))

