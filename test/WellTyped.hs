{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}

module Main (module Main) where

import WidgetRattus
import WidgetRattus.Signal
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


-- check whether the Stable constraint solver handles GADTs correctly.

data Fun a where
  Fun :: Stable s => !s -> !(Box(s -> Int -> (s :* a))) -> Fun a


newtype Beh a = Beh (Sig (Fun a))

funTest :: Fun a -> O () -> O (Fun a)
funTest fun@(Fun x f) d = delay (let _ = adv d in x `seq` fun)

funTest2 :: Fun a -> O () -> O (Fun a)
funTest2 fun d = case fun of (!(Fun x f)) -> delay (let _ = adv d in x `seq` fun)

funTest3 :: C (Fun a) -> O () -> C (O (Fun a))
funTest3 fun d = do Fun x f <-  fun 
                    fun' <- fun
                    return (delay (let _ = adv d in x `seq` fun'))

funTest4 :: Fun a -> O () -> C (O (Fun a))
funTest4 fun@(Fun x f) d = do let (x':* v) = unbox f x 0
                              return (delay (let _ = adv d in x' `seq` fun))



funTest5 :: Fun a -> O () -> O (Fun a)
funTest5 fun@(Fun x f) d = delay (let _ = adv d in x' `seq` fun)
  where (x':* v) = unbox f x 0


funTest6 :: Fun a -> O () -> O (Fun a)
funTest6 fun@(Fun x f) d = let (x':* v) = unbox f x 0 in delay (let _ = adv d in x' `seq` fun)



funTestWorkaround :: Fun a -> O () -> O (Fun a)
funTestWorkaround fun@(Fun x f) d = foo x fun
  where foo :: Stable s => s -> (Fun a) -> O (Fun a)
        foo x fun = delay (let _ = adv d in x `seq` fun)

zipFun :: Box (a -> b -> c) -> Fun a -> Fun b -> Fun c
zipFun f (Fun sa fa) (Fun sb fb) = Fun (sa :* sb) 
  (box (\ (sa' :* sb') t -> 
          let (sa'' :* a) = unbox fa sa' t
              (sb'' :* b) = unbox fb sb' t
          in ((sa'' :* sb'') :* unbox f a b) ))
                      

zipWithBeh :: (Stable a, Stable b) => Box (a -> b -> c) -> Beh a -> Beh b -> Beh c
zipWithBeh f (Beh as) (Beh bs) = Beh (run as bs) where
  run (a ::: as) (b ::: bs) = zipFun f a b ::: delay 
     (case select as bs of
        Fst as' lbs -> run as' (b ::: lbs)
        Snd las bs' -> run (a ::: las) bs'
        Both as' bs' -> run as' bs')

main = putStrLn "This file should just type check"
