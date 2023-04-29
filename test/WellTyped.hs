{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RebindableSyntax #-}

module Main (module Main) where

import Rattus
import Rattus.Stream
import Rattus.Yampa
import Data.Set as Set
import Prelude


{-# ANN module Rattus #-}

boxedInt :: Box Int
boxedInt = box 8

lambdaUnderDelay :: O (Int -> Int -> Int)
lambdaUnderDelay = delay (\x _ -> x)

sneakyLambdaUnderDelay :: O (Int -> Int -> Int)
sneakyLambdaUnderDelay = delay (let f x _ =  x in f)


lambdaUnderDelay' :: Int -> O (Int -> Int)
lambdaUnderDelay' x = delay (\_ -> x)

sneakyLambdaUnderDelay' :: Int -> O (Int -> Int)
sneakyLambdaUnderDelay' x = delay (let f _ =  x in f)

scanBox :: Box(b -> a -> Box b) -> b -> Str a -> Str b
scanBox f acc (a ::: as) =  unbox acc' ::: delay (scanBox f (unbox acc') (adv as))
  where acc' = unbox f acc a


sumBox :: Str Int -> Str Int
sumBox = scanBox (box (\x y -> box' (x + y))) 0

map1 :: Box (a -> b) -> Str a -> Str b
map1 f (x ::: xs) = unbox f x ::: delay (map1 f (adv xs))

map2 :: Box (a -> b) -> Str a -> Str b
map2 f (x ::: xs) = unbox f x ::: (delay map2 <## f <#> xs)

map3 :: Box (a -> b) -> Str a -> Str b
map3 f = run
  where run (x ::: xs) = unbox f x ::: (delay run <#> xs)


-- local mutual recursive definition
nestedMutual :: Str Int -> Str Int
nestedMutual = lbar1 (box (+1))
  where lbar1 :: Box (a -> b) -> Str a -> Str b
        lbar1 f (x ::: xs) = unbox f x ::: (delay (lbar2 f) <#> xs)

        lbar2 :: Box (a -> b) -> Str a -> Str b
        lbar2 f  (x ::: xs) = unbox f x ::: (delay (lbar1 f) <#> xs)



-- mutual recursive definition
bar1 :: Box (a -> b) -> Str a -> Str b
bar1 f (x ::: xs) = unbox f x ::: (delay (bar2 f) <#> xs)

bar2 :: Box (a -> b) -> Str a -> Str b
bar2 f  (x ::: xs) = unbox f x ::: (delay (bar1 f) <#> xs)


applyDelay :: O (O (a -> b)) -> O (O a) -> O (O b)
applyDelay f x = delay (adv f <#> adv x)


stableDelay :: Stable a => a -> O a
stableDelay x = delay x

patternBinding :: Str Int -> Str Int
patternBinding str = (x + 1) ::: (delay patternBinding <#> xs)
  where (x ::: xs) = sumBox str


data Input a = Input {jump :: !a, move :: !Move}
data Move = StartLeft | EndLeft | StartRight | EndRight | NoMove

type Inp a b = Input a



-- The compiler plugin should detect that Input is a stable type and
-- thus remains in scope under the delay.
constS :: Stable a => (Inp a b) -> Str (Inp a b)
constS a = a ::: delay (constS a)

-- The constraint solver plugin should detect that Input is a stable
-- type and thus 'const1' can be instantiated.
constS' :: Stable a => Inp a b -> Str (Inp a b)
constS' = const1

-- make sure that unit is recognized as stable
constU :: () -> Str ()
constU a = a ::: delay (constU a)

constU' :: () -> Str ()
constU' = const1


const1 :: Stable a => a -> Str a
const1 a = a ::: delay (const1 a)

const2 :: Stable a => a -> Str a
const2 a = run
  where run = a ::: delay run

scan1 :: (Stable b) => Box(b -> a -> b) -> b -> Str a -> Str b
scan1 f acc (a ::: as) =  acc' ::: delay (scan1 f acc' (adv as))
  where acc' = unbox f acc a

scan2 :: (Stable b) => Box(b -> a -> b) -> b -> Str a -> Str b
scan2 f = run
  where run acc (a ::: as) = let acc' = unbox f acc a
                             in acc' ::: delay (run acc' (adv as))

scanSet :: Str Int -> Str (Set Int)
scanSet = scan1 (box (\ s x -> Set.insert x s)) Set.empty

from :: Int -> Str Int
from n  = n ::: delay (from (n+1))

alt :: Int -> Int -> Str Int
alt n m = n ::: delay (alt m n)


myMap :: Str Int -> Str Int
myMap (x ::: xs) = (x + 1) ::: delay (fst' (myMap (adv xs):*nats))

nats :: Str Int
nats = 0 ::: delay (myMap nats)



{-# ANN main NotRattus #-}
main = putStrLn "This file should just type check"
