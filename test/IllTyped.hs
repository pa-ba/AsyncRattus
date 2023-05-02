{-# LANGUAGE RebindableSyntax #-}

module Main (module Main) where

import Rattus
import Rattus.Stream as S
import Prelude
import Rattus.Plugin.Annotation (InternalAnn (..))


{-# ANN module Rattus #-}


{-# ANN loopIndirect ExpectTcError #-}
loopIndirect :: Str v Int
loopIndirect = run
  where run :: Str v Int
        run = loopIndirect

{-# ANN loopIndirect' ExpectTcError #-}
loopIndirect' :: Str v Int
loopIndirect' = let run = loopIndirect' in run

{-# ANN nestedUnguard ExpectTcError #-}
nestedUnguard :: Str v Int
nestedUnguard = run 0
  where run :: Int -> Str v Int
        run 0 = nestedUnguard
        run n = n ::: delay (run (n-1))

{-# ANN advDelay ExpectTcError #-}
advDelay :: O v (O v a) -> O v a
advDelay y = delay (let x = adv y in adv x)

{-# ANN advDelay' ExpectTcError #-}
advDelay' :: O v a -> a
advDelay' y = let x = adv y in x

{-# ANN dblAdv ExpectTcError #-}
dblAdv :: O v (O v a) -> O v a
dblAdv y = delay (adv (adv y))

{-# ANN advScope ExpectTcError #-}
advScope :: O v (O v Int -> Int)
advScope = delay (\x -> adv x)

{-# ANN advScope' ExpectTcError #-}
advScope' :: O v (Int -> Int)
advScope' = delay (let f x =  adv (delay x) in f)

{-# ANN grec ExpectTcError #-}
grec :: a
grec = grec

{-# ANN boxStream ExpectTcError #-}
boxStream :: Str v Int -> Box (Str v Int)
boxStream s = box (0 ::: tl s)

{-# ANN boxStream' ExpectTcError #-}
boxStream' :: Str v Int -> Box (Str v Int)
boxStream' s = box s

{-# ANN intDelay ExpectTcError #-}
intDelay :: Int -> O v Int
intDelay = delay

{-# ANN intAdv ExpectTcError #-}
intAdv :: O v Int -> Int
intAdv = adv


{-# ANN newDelay ExpectTcError #-}
newDelay :: a -> O v a
newDelay x = delay x

{-# ANN mutualLoop ExpectTcError #-}
mutualLoop :: a
mutualLoop = mutualLoop'

{-# ANN mutualLoop' ExpectTcError #-}
mutualLoop' :: a
mutualLoop' = mutualLoop

{-# ANN constUnstable ExpectTcError #-}
constUnstable :: a -> Str v a
constUnstable a = run
  where run = a ::: delay run

{-# ANN mapUnboxed ExpectTcError #-}
mapUnboxed :: (a -> b) -> Str v a -> Str v b
mapUnboxed f = run
  where run (x ::: xs) = f x ::: delay (run (adv xs))

{-# ANN mapUnboxedMutual ExpectTcError #-}
mapUnboxedMutual :: (a -> b) -> Str v a -> Str v b
mapUnboxedMutual f = run
  where run (x ::: xs) = f x ::: delay (run' (adv xs))
        run' (x ::: xs) = f x ::: delay (run (adv xs))

-- mutual recursive pattern definitions are not supported
-- foo1,foo2 :: Box (a -> b) -> Str a -> Str b
-- (foo1,foo2) = (\ f (x ::: xs) -> unbox f x ::: (delay (foo2 f) <#> xs),
--                \ f (x ::: xs) -> unbox f x ::: (delay (foo1 f) <#> xs))

{-# ANN nestedPattern ExpectTcError #-}
nestedPattern :: Box (a -> b) -> Str v a -> Str v b
nestedPattern = foo1 where
  foo1,foo2 :: Box (a -> b) -> Str v a -> Str v b
  (foo1,foo2) = (\ f (x ::: xs) -> unbox f x ::: (delay (foo2 f (adv xs))),
                 \ f (x ::: xs) -> unbox f x ::: (delay (foo1 f (adv xs))))


data Input = Input {jump :: !Bool, move :: Move}
data Move = StartLeft | EndLeft | StartRight | EndRight | NoMove

{-# ANN constS ExpectTcError #-}
-- Input is not a stable type (it is not strict). Therefore this
-- should not type check.
constS :: Input -> Str v Input
constS a = a ::: delay (constS a)


-- Since Input is not strict, we cannot instantiate the 'const'
-- function.
-- Uncomment the definition below to check this.

-- constS' :: Input -> Str Input
-- constS' = const


{-# ANN main NotRattus #-}
main = putStrLn "This file should not type check"
