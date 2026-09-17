{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}

module Main (module Main) where

import AsyncRattus
import AsyncRattus.Signal
import Data.Set as Set
import Data.Text
import qualified Data.String as Str
import qualified GHC.Exts as Exts

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

-- Check that scope checking accounts for the Stable constraint that
-- pattern matching on an existential/GADT constructor brings into
-- scope. In each case the existentially bound x must remain in scope
-- under the delay.

-- match in a function definition
funTest :: Fun a -> O () -> O (Fun a)
funTest fun@(Fun x _) d = delay (let _ = adv d in x `seq` fun)

-- match in a case expression
funTest2 :: Fun a -> O () -> O (Fun a)
funTest2 fun = case fun of Fun x _ -> \ d -> delay (let _ = adv d in x `seq` fun)

-- the stable constraint must reach a where-bound pattern binding
funTest5 :: Fun a -> O () -> O (Fun a)
funTest5 fun@(Fun x f) d = delay (let _ = adv d in x' `seq` fun)
  where (x' :* _) = unbox f x 0

-- ... and a let-bound pattern binding
funTest6 :: Fun a -> O () -> O (Fun a)
funTest6 fun@(Fun x f) d =
  let (x' :* _) = unbox f x 0 in delay (let _ = adv d in x' `seq` fun)

-- ... and a pattern guard
funTestGuard :: Fun a -> O () -> O (Fun a)
funTestGuard fun d
  | Fun x _ <- fun = delay (let _ = adv d in x `seq` fun)

-- ... and a bind statement in do notation
{-# ANN funTestBind AllowLazyData #-}
funTestBind :: Maybe (Fun a) -> O () -> Maybe (O (Fun a))
funTestBind fun d = do Fun x _ <- fun
                       fun' <- fun
                       return (delay (let _ = adv d in x `seq` fun'))

-- this workaround was previously needed to get the above to compile
funTestWorkaround :: Fun a -> O () -> O (Fun a)
funTestWorkaround fun@(Fun x _) d = foo x fun
  where foo :: Stable s => s -> Fun a -> O (Fun a)
        foo y g = delay (let _ = adv d in y `seq` g)


-- check that newtypes over stable types are recognised as stable

newtype Count = Count Int

newtypeStable :: Count -> O () -> O Count
newtypeStable x d = delay (let _ = adv d in x)


-- check the strict sum type

strictSum :: Int :+ Bool -> Int
strictSum (Left' n) = n
strictSum (Right' b) = if b then 1 else 0

strictSumStable :: Int :+ Bool -> O () -> O (Int :+ Bool)
strictSumStable x d = delay (let _ = adv d in x)


-- check the Functor instance of Maybe'

incMaybe' :: Maybe' Int -> Maybe' Int
incMaybe' = fmap (+1)


-- The definitions below must not produce a "may lead to memory leaks"
-- warning: the lazy arguments of fromString, fromList/fromListN and
-- Data.Text.pack are consumed immediately and are not retained. Note
-- that the arguments must not be literals, since those are already
-- exempt from the check.

-- fromListN, as inserted by OverloadedLists
intSet :: Set Int
intSet = [1,2,3]

-- fromList, the method of the IsList class
setFromList :: [Int] -> Set Int
setFromList xs = Exts.fromList xs

-- fromString, the method of the IsString class
textFromString :: String -> Text
textFromString s = Str.fromString s

-- Data.Text.pack
packedText :: String -> Text
packedText s = pack s


-- 'Item l' must be recognised as strict whenever 'l' is.

itemStrict :: IsList l => l -> Item l -> List (Item l)
itemStrict _ x = x :! Nil


main = putStrLn "This file should just type check"
