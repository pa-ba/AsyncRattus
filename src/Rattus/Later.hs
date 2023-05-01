{-# OPTIONS -fplugin=Rattus.Plugin #-}

module Rattus.Later (
    map,
    selectMany,
) where

import Prelude hiding (map, Left, Right)
import Rattus
import Rattus.Primitives ( O, delay, adv, Select(..), never)
import Rattus.Strict (List(..), singleton)

{-# ANN module Rattus #-}

map :: Box (a -> b) -> O v a -> O v b
map f later = delay (unbox f (adv later))

-- Given a list of delayed values, select over them return list of all available values when one arrives.
selectMany :: List (O v a) -> O v (List (Int, a))
selectMany = selectMany' 0

{-# ANN selectMany' AllowRecursion #-}
selectMany' :: Int -> List (O v a) -> O v (List (Int, a))
selectMany' _ Nil = never
selectMany' _ (x :! Nil) = delay ((0, adv x) :! Nil)
selectMany' n (x :! y :! Nil) = 
    delay (
        case select x y of
            Both a b -> (n, a) :! (n+1, b) :! Nil
            Left a lb -> singleton (n, a)
            Right la b -> singleton (n+1, b)
    )
selectMany' n (x :! xs) =
    let xs' = selectMany' (n+1) xs
    in
    delay (
        case select x xs' of
            Both a b -> (n, a) :! b
            Left a lb -> singleton (n, a)
            Right la b -> b
    )
