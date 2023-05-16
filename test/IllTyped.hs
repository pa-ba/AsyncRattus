{-# LANGUAGE RebindableSyntax #-}

module Main (module Main) where

import AsyncRattus
import AsyncRattus.Stream as S
import Prelude
import AsyncRattus.Plugin.Annotation (InternalAnn (..))


{-# ANN module AsyncRattus #-}


{-# ANN loopIndirect ExpectScopeError #-}
loopIndirect :: Str Int
loopIndirect = run
  where run :: Str Int
        run = loopIndirect

{-# ANN loopIndirect' ExpectScopeError #-}
loopIndirect' :: Str Int
loopIndirect' = let run = loopIndirect' in run

{-# ANN nestedUnguard ExpectScopeError #-}
nestedUnguard :: Str Int
nestedUnguard = run 0
  where run :: Int -> Str Int
        run 0 = nestedUnguard
        run n = n ::: delay (run (n-1))

{-# ANN advDelay ExpectScopeError #-}
advDelay :: O (O a) -> O a
advDelay y = delay (let x = adv y in adv x)

{-# ANN advDelay' ExpectScopeError #-}
advDelay' :: O a -> a
advDelay' y = let x = adv y in x

{-# ANN dblAdv ExpectScopeError #-}
dblAdv :: O (O a) -> O a
dblAdv y = delay (adv (adv y))

{-# ANN advScope ExpectScopeError #-}
advScope :: O (O Int -> Int)
advScope = delay (\x -> adv x)

{-# ANN advScope' ExpectScopeError #-}
advScope' :: O (Int -> Int)
advScope' = delay (let f x =  adv (delay x) in f)

{-# ANN grec ExpectScopeError #-}
grec :: a
grec = grec

{-# ANN boxStream ExpectScopeError #-}
boxStream :: Str Int -> Box (Str Int)
boxStream s = box (0 ::: tl s)

{-# ANN boxStream' ExpectScopeError #-}
boxStream' :: Str Int -> Box (Str Int)
boxStream' s = box s

{-# ANN intDelay ExpectScopeError #-}
intDelay :: Int -> O Int
intDelay = delay

{-# ANN intAdv ExpectScopeError #-}
intAdv :: O Int -> Int
intAdv = adv


{-# ANN newDelay ExpectScopeError #-}
newDelay :: a -> O a
newDelay x = delay x

{-# ANN mutualLoop ExpectScopeError #-}
mutualLoop :: a
mutualLoop = mutualLoop'

{-# ANN mutualLoop' ExpectScopeError #-}
mutualLoop' :: a
mutualLoop' = mutualLoop

{-# ANN constUnstable ExpectScopeError #-}
constUnstable :: a -> Str a
constUnstable a = run
  where run = a ::: delay run

{-# ANN mapUnboxed ExpectScopeError #-}
mapUnboxed :: (a -> b) -> Str a -> Str b
mapUnboxed f = run
  where run (x ::: xs) = f x ::: delay (run (adv xs))

{-# ANN mapUnboxedMutual ExpectScopeError #-}
mapUnboxedMutual :: (a -> b) -> Str a -> Str b
mapUnboxedMutual f = run
  where run (x ::: xs) = f x ::: delay (run' (adv xs))
        run' (x ::: xs) = f x ::: delay (run (adv xs))

-- mutual recursive pattern definitions are not supported
-- foo1,foo2 :: Box (a -> b) -> Str a -> Str b
-- (foo1,foo2) = (\ f (x ::: xs) -> unbox f x ::: (delay (foo2 f) <#> xs),
--                \ f (x ::: xs) -> unbox f x ::: (delay (foo1 f) <#> xs))

{-# ANN nestedPattern ExpectScopeError #-}
nestedPattern :: Box (a -> b) -> Str a -> Str b
nestedPattern = foo1 where
  foo1,foo2 :: Box (a -> b) -> Str a -> Str b
  (foo1,foo2) = (\ f (x ::: xs) -> unbox f x ::: (delay (foo2 f (adv xs))),
                 \ f (x ::: xs) -> unbox f x ::: (delay (foo1 f (adv xs))))


data Input = Input {jump :: !Bool, move :: Move}
data Move = StartLeft | EndLeft | StartRight | EndRight | NoMove

{-# ANN constS ExpectScopeError #-}
-- Input is not a stable type (it is not strict). Therefore this
-- should not type check.
constS :: Input -> Str Input
constS a = a ::: delay (constS a)


-- Since Input is not strict, we cannot instantiate the 'const'
-- function.
-- Uncomment the definition below to check this.

-- constS' :: Input -> Str Input
-- constS' = const

{-# ANN incompatibleAdv ExpectClockError #-}
incompatibleAdv :: O Int -> O Int -> O Int
incompatibleAdv li lk = delay (adv li + adv lk)

{-# ANN incompatibleAdvSelect ExpectClockError #-}
incompatibleAdvSelect :: O Int -> O Int -> O Int
incompatibleAdvSelect li lk = delay (adv li + adv lk)

{-# ANN intPlusOne ExpectScopeError #-}
intPlusOne :: O Int -> Int
intPlusOne laterI = adv laterI + 1

{-# ANN weirdPlusTwo ExpectScopeError #-}
weirdPlusTwo :: O Int -> O Int
weirdPlusTwo x = delay (
        let doAdd = box ((+) 1)
            x' = x
            newLater = delay (unbox doAdd (adv x'))
        in unbox doAdd (adv newLater)
    )

{-# ANN stutter ExpectClockError #-}
stutter :: Int -> Str Int
stutter n = n ::: delay (n ::: delay (stutter (n+1)))

{-# ANN advAlias ExpectScopeError #-}
advAlias :: O a -> a
advAlias = adv

{-# ANN selectAlias ExpectScopeError #-}
selectAlias :: O a -> O b -> Select a b
selectAlias = select

{-# ANN partialSelectApp ExpectScopeError #-}
partialSelectApp :: O a -> (O b -> Select a b)
partialSelectApp l = select l

{-# ANN main NotAsyncRattus #-}
main = putStrLn "This file should not type check"
