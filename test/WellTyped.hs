{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}

module Main (module Main) where

import AsyncRattus
import AsyncRattus.Signal
import Data.Set as Set
import Data.Text

boxedInt :: Box Int
boxedInt = box 8


lambdaUnderDelay :: O Int -> O ((Int -> Int -> Int) :* Int)
lambdaUnderDelay d = delay ((\x _ -> x) :* adv d)

sneakyLambdaUnderDelay :: O Int -> O ((Int -> Int -> Int) :* Int)
sneakyLambdaUnderDelay d = delay (let f x _ =  x in f :* adv d)


lambdaUnderDelay' :: Int -> O Int -> O ((Int -> Int) :* Int)
lambdaUnderDelay' x d = delay ((\_ -> x) :* adv d)

sneakyLambdaUnderDelay' :: Int -> O Int -> O ((Int -> Int) :* Int)
sneakyLambdaUnderDelay' x d = delay ((let f _ =  x in f) :* adv d)


scanBox :: Box(b -> a -> Box b) -> b -> Sig a -> Sig b
scanBox f acc (a ::: as) =  unbox acc' ::: delay (scanBox f (unbox acc') (adv as))
  where acc' = unbox f acc a

sumBox :: Sig Int -> Sig Int
sumBox = scanBox (box (\x y -> box (x + y))) 0

strMap :: Box (a -> b) -> Sig a -> Sig b
strMap f (x ::: xs) = unbox f x ::: delay (strMap f (adv xs))

strMap' :: Box (a -> b) -> Sig a -> Sig b
strMap' f = run
  where run (x ::: xs) = unbox f x ::: delay (run (adv xs))



-- local mutual recursive definition
nestedMutual :: Sig Int -> Sig Int
nestedMutual = lbar1 (box (+1))
  where lbar1 :: Box (a -> b) -> Sig a -> Sig b
        lbar1 f (x ::: xs) = unbox f x ::: (delay (lbar2 f (adv xs)))

        lbar2 :: Box (a -> b) -> Sig a -> Sig b
        lbar2 f  (x ::: xs) = unbox f x ::: (delay (lbar1 f (adv xs)))



-- mutual recursive definition
bar1 :: Box (a -> b) -> Sig a -> Sig b
bar1 f (x ::: xs) = unbox f x ::: delay (bar2 f (adv xs))

bar2 :: Box (a -> b) -> Sig a -> Sig b
bar2 f  (x ::: xs) = unbox f x ::: delay (bar1 f (adv xs))

stableDelay :: Stable a => Box (a -> a -> a) -> a -> O a -> O a
stableDelay f v l = delay (unbox f v (adv l))

patternBinding :: Sig Int -> Sig Int
patternBinding str = (x + 1) ::: (delay (patternBinding (adv xs)))
  where (x ::: xs) = sumBox str


data Input a = Input {jump :: !a, move :: !Move}
data Move = StartLeft | EndLeft | StartRight | EndRight | NoMove



-- The compiler plugin should detect that Input is a stable type and
-- thus remains in scope under the delay.
constS :: Stable a => Input a -> O Int -> Sig (Int :* Input a)
constS a l = (0 :* a) ::: delay ((adv l :* a) ::: never)

-- make sure that unit is recognized as stable
constU :: () -> O () -> Sig (() :* ())
constU a l = (() :* a) ::: delay ((adv l :* a) ::: never)


scan1 :: (Stable b) => Box(b -> a -> b) -> b -> Sig a -> Sig b
scan1 f acc (a ::: as) =  acc' ::: delay (scan1 f acc' (adv as))
  where acc' = unbox f acc a

scan2 :: (Stable b) => Box(b -> a -> b) -> b -> Sig a -> Sig b
scan2 f = run
  where run acc (a ::: as) = let acc' = unbox f acc a
                             in acc' ::: delay (run acc' (adv as))

scanSet :: Sig Int -> Sig (Set Int)
scanSet = scan1 (box (\ s x -> Set.insert x s)) Set.empty

myMap :: Sig Int -> Sig Int
myMap (x ::: xs) = (x + 1) ::: delay (fst' (myMap (adv xs) :* nats never))

nats :: O Int -> Sig Int
nats l = 0 ::: delay (let (n ::: ns) = myMap (nats never) in (adv l + n) ::: ns)

nestedDelay :: Sig a -> Sig a
nestedDelay (a ::: as) = a ::: delay (let x ::: xs = adv as in x ::: delay (nestedDelay (adv xs)))


naiveIf :: Bool -> O a -> O a -> O (Bool :* a)
naiveIf b x y = delay (b :* adv (if b then x else y))

naiveIf' :: Bool -> O a -> O a -> O (Bool :* a)
naiveIf' b x y = delay (b :* adv later)
    where
        later = case b of
            True -> x
            False -> y

advUnderLambda :: O Int -> O (a -> Int)
advUnderLambda y = delay (\_ -> adv y)


stableText :: Text -> Sig Text -> Sig Text
stableText = scan (box append) 

stableInteger :: Integer -> Sig Integer -> Sig Integer
stableInteger = scan (box (+)) 


dblAdv :: O (O a) -> O (O a)
dblAdv y = delay (delay (adv (adv y)))

delayAdvUnderLambda :: O () -> O (O Int -> O Int)
delayAdvUnderLambda d = delay (adv d `seq` \x -> delay (adv x))

-- This function is leaky unless the single tick transformation is
-- performed
leaky :: Sig () -> (() -> Bool) -> Sig Bool
leaky (() ::: d) p = p () ::: delay (let d' = adv d in (leaky d' (\ _ -> current (leaky d' (\ _ -> True)))))

unusedAdv :: O () -> O ()
unusedAdv d = delay (adv d `seq` ())

unusedAdv' :: O () -> O ()
unusedAdv' d = delay (let _ = adv d in ())

progressDelay :: Sig Int -> Sig Bool -> O () -> O (Sig Int :* Sig Bool)
progressDelay xs ys x = delay (adv x `seq` (progress xs :* progress ys))

progressListDelay :: List (Sig Int)  -> O () -> O (List (Sig Int))
progressListDelay xs x = delay (adv x `seq` progress xs)

main = putStrLn "This file should just type check"
