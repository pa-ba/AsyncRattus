{-# LANGUAGE TypeOperators #-}

module Main (module Main) where

import Rattus
import Rattus.Stream as Str
import Rattus.Plugin.Annotation (InternalAnn (..))
{-
{-# ANN module Rattus #-}


-- | This function should be fine (no warnings, no time-leaks)
nats' :: Str Int
nats' = unfold (box ((+) 1)) 0

-- | This function should be fine (no warnings, no time-leaks)
nats :: Str Int
nats = from  0
    where from :: Int -> Str Int
          from n = n ::: delay (from (n+1))

  -- This function should cause a warning.
{-# ANN leakyNats ExpectWarning #-}
leakyNats :: Str Int
leakyNats = 0 ::: delay (Str.map (box (+1)) leakyNats)

inc :: Str Int -> Str Int
inc = Str.map (box ((+)1))

leakyNats' :: Str Int
leakyNats' = 0 ::: delay (inc leakyNats')



altMap :: Box (a -> b) -> Box (a -> b) -> Str a -> Str b
altMap f g (x ::: xs) = unbox f x ::: delay (altMap g f (adv xs))

-- This function should cause a warning.
{-# ANN leakyAlt ExpectWarning #-}
leakyAlt :: Str Int
leakyAlt = 0 ::: delay (altMap (box ((+)1)) (box ((+)2)) leakyAlt)

mapMap :: Box (a -> a) -> Box (a -> a) -> Str a -> Str a
mapMap f g (x ::: xs) = unbox f x ::: delay (Str.map g (mapMap g f (adv xs)))

-- This function should cause a warning.
{-# ANN leakyMap ExpectWarning #-}
leakyMap :: Str Int
leakyMap = 0 ::: delay (mapMap (box ((+)1)) (box ((+)2)) leakyMap)



-- This function should cause a warning.
{-# ANN boxLeaky ExpectWarning #-}
boxLeaky :: Str Int
boxLeaky = run (box (\() -> 1)) 1
  where run :: Box (() -> Int) -> Int -> Str Int
        run f a = (if a == 0 then unbox f () else a)
                  ::: delay (run (box (\ () -> (unbox f () + 1))) a)

-- This function should cause a warning.
{-# ANN boxLeaky' ExpectWarning #-}
boxLeaky' :: Str Int -> Str Int
boxLeaky' = run (box (\() -> 1)) 1
  where run :: Box (() -> Int) -> Int -> Str Int -> Str Int
        run f a (x ::: xs) = (if a == 0 then unbox f () else a) ::: delay (run (box (\ () -> (unbox f () + x))) a (adv xs))

-- This function should cause a warning.
{-# ANN natsTrans ExpectWarning #-}
natsTrans :: Str Int -> Str Int
natsTrans (x ::: xs) = x ::: delay (Str.map (box ((+)x)) $ natsTrans $ adv xs)

-- This function should cause a warning.
{-# ANN leakySum ExpectWarning #-}
leakySum :: Box (Int -> Int) -> Str Int -> Str Int
leakySum f (x ::: xs) = unbox f x ::: (delay (leakySum (box (\ y -> unbox f (y + x)))) <#> xs)


{-# ANN recurse NotRattus #-}
recurse 0 (x : _) = print x
recurse n (_ : xs) = recurse (n-1) xs
recurse _ [] = putStrLn "the impossible happened: stream terminated"

{-# ANN main NotRattus #-}
main = do
  let x = fromStr nats
  recurse 10000000 x
    
  let x = fromStr leakyNats
  recurse 10000000 x

  let x = fromStr leakyNats'
  recurse 10000000 x

  let x = fromStr leakyAlt
  recurse 10000000 x

  let x = fromStr leakyMap
  recurse 10000 x

  let x = fromStr $ natsTrans (toStr [1..])
  recurse 10000 x

  let x = fromStr $ boxLeaky' (toStr [1..])
  recurse 10000000 x

  let x = fromStr boxLeaky
  recurse 10000000 x
-}

main = putStrLn ""
