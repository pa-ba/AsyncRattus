{-# OPTIONS -fplugin=Rattus.Plugin #-}

module Rattus.Later (
    map,
    selectMany,
) where

import Prelude hiding (map, Left, Right)
import Rattus
import Rattus.Primitives ( O, delay, adv, Select(..))
import Rattus.Strict (List(..), singleton)

{-# ANN module Rattus #-}

map :: (a -> b) -> O v a -> O v b
map f later = delay (f (adv later))

-- Given a list of delayed values, select over them return list of all available values when one arrives.
selectMany :: List (O v a) -> O v (List (Int, a))
selectMany lst = aux 0 lst
    where
        aux n (x :! y :! Nil) = 
            delay (
                case select x y of
                    Both a b -> (n, a) :! (n+1, b) :! Nil
                    Left a lb -> singleton (n, a)
                    Right la b -> singleton (n+1, b)
            )
        aux n (x :! xs) =
            delay (
                let xs' = (aux (n+1) xs) in
                case select x xs' of
                    Both a b -> (n, a) :! b
                    Left a lb -> singleton (n, a)
                    Right la b -> b
            )
        