{-# LANGUAGE TypeOperators #-}

module Main (module Main) where

import AsyncRattus
import AsyncRattus.Stream
import Data.Set as Set
import Prelude


{-# ANN module AsyncRattus #-}

boxedInt :: Box Int
boxedInt = box 8

scanBox :: Box(b -> a -> Box b) -> b -> Str v a -> Str v b
scanBox f acc (a ::: as) =  unbox acc' ::: delay (scanBox f (unbox acc') (adv as))
  where acc' = unbox f acc a


sumBox :: Str v Int -> Str v Int
sumBox = scanBox (box (\x y -> box' (x + y))) 0

strMap :: Box (a -> b) -> Str v a -> Str v b
strMap f (x ::: xs) = unbox f x ::: delay (strMap f (adv xs))


-- local mutual recursive definition
nestedMutual :: Str v Int -> Str v Int
nestedMutual = lbar1 (box (+1))
  where lbar1 :: Box (a -> b) -> Str v a -> Str v b
        lbar1 f (x ::: xs) = unbox f x ::: (delay (lbar2 f (adv xs)))

        lbar2 :: Box (a -> b) -> Str v a -> Str v b
        lbar2 f  (x ::: xs) = unbox f x ::: (delay (lbar1 f (adv xs)))



-- mutual recursive definition
bar1 :: Box (a -> b) -> Str v a -> Str v b
bar1 f (x ::: xs) = unbox f x ::: delay (bar2 f (adv xs))

bar2 :: Box (a -> b) -> Str v a -> Str v b
bar2 f  (x ::: xs) = unbox f x ::: delay (bar1 f (adv xs))

stableDelay :: Stable a => Box (a -> a -> a) -> a -> O v a -> O v a
stableDelay f v l = delay (unbox f v (adv l))

patternBinding :: Str v Int -> Str v Int
patternBinding str = (x + 1) ::: (delay (patternBinding (adv xs)))
  where (x ::: xs) = sumBox str


data Input a = Input {jump :: !a, move :: !Move}
data Move = StartLeft | EndLeft | StartRight | EndRight | NoMove

type Inp a b = Input a



-- The compiler plugin should detect that Input is a stable type and
-- thus remains in scope under the delay.
constS :: Stable a => (Inp a b) -> O v Int -> Str v (Int, Inp a b)
constS a l = (0, a) ::: delay ((adv l, a) ::: never)

-- make sure that unit is recognized as stable
constU :: () -> O v () -> Str v ((), ())
constU a l = ((), a) ::: delay ((adv l, a) ::: never)


scan1 :: (Stable b) => Box(b -> a -> b) -> b -> Str v a -> Str v b
scan1 f acc (a ::: as) =  acc' ::: delay (scan1 f acc' (adv as))
  where acc' = unbox f acc a

scan2 :: (Stable b) => Box(b -> a -> b) -> b -> Str v a -> Str v b
scan2 f = run
  where run acc (a ::: as) = let acc' = unbox f acc a
                             in acc' ::: delay (run acc' (adv as))

scanSet :: Str v Int -> Str v (Set Int)
scanSet = scan1 (box (\ s x -> Set.insert x s)) Set.empty

myMap :: Str v Int -> Str v Int
myMap (x ::: xs) = (x + 1) ::: delay (fst' (myMap (adv xs) :* nats never))

nats :: O v Int -> Str v Int
nats l = 0 ::: delay (let (n ::: ns) = myMap (nats never) in (adv l + n) ::: ns)

--stutter :: (Stable a) => Str v a -> Str v a
--stutter (a ::: as) = a ::: delay (a ::: delay (adv as))

-- should work since we can extract the clock of the variable at runtime.
-- because of the toSingleTick pass, the result of the if-expression is bound to a new variable, so we cannot
-- distinguish between a legal variable and a synthesized one.
naiveIf :: Bool -> O v a -> O v a -> O v (Bool, a)
naiveIf b x y = delay (b, adv (if b then x else y))

naiveIf' :: Bool -> O v a -> O v a -> O v (Bool, a)
naiveIf' b x y = delay (b, adv later)
    where
        later = case b of
            True -> x
            False -> y

doubleAdv :: O v Int -> O v Int
doubleAdv li = 
  delay (
    adv li + adv li
  )

advOnAliasedVar :: O v Int -> O v Int
advOnAliasedVar li =
  let lk = li
  in delay (
    adv li + adv lk
  )

advUnderLambda :: O v Int -> O v (a -> Int)
advUnderLambda y = delay (\x -> adv y)

{-# ANN main NotAsyncRattus #-}
main = putStrLn "This file should just type check"
