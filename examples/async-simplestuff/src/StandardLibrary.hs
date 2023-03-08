{-# LANGUAGE TypeOperators #-}

module Main where

import Rattus
import Rattus.Stream
import Rattus.ToHaskell
import Rattus.Primitives
import qualified Data.Set as Set

{-# ANN module Rattus #-}

mapL :: Box (a -> b) -> O a -> O b
mapL f (Delay cl inpF) = Delay cl (unbox f . inpF)

mapS :: Box (a -> b) -> Str a -> Str b
mapS f (a ::: as) = unbox f a ::: delay (mapL (mapS f as))

main2 :: IO ()
main2 = do
    print (adv' (1, IntValue 0) laterB)
    where laterA = delayCustom (Set.singleton 1) id
          laterInt = mapL (box (\ (chId, IntValue i) -> i)) laterA
          laterB = mapL (box (>1)) laterInt

main :: IO ()
main = do
    print (take 10 $ fromStr str')
    where str' = mapS (box (*2)) (toStr [1..])