{-# LANGUAGE RebindableSyntax #-}

module Main (module Main) where

import Rattus
import Rattus.Stream as S
import Prelude
import Rattus.Plugin.Annotation (InternalAnn (..))

-- All of these examples should typecheck with the more relaxed typing
-- rules of Rattus that allows functions and delays under tick.
{-
{-# ANN module Rattus #-}

{-# ANN recBox ExpectWarning #-}
recBox :: Str Int
recBox = 0 ::: unbox (box (delay recBox))


dblDelay :: O (O Int)
dblDelay = delay (delay 1)


dblAdv :: O (O a) -> O (O a)
dblAdv y = delay (delay (adv (adv y)))


lambdaUnderDelay :: O (Int -> Int)
lambdaUnderDelay = delay (\x -> x)

delayAdvUnderLambda :: O (O Int -> O Int)
delayAdvUnderLambda = delay (\x -> delay (adv x))

sneakyLambdaUnderDelay' :: O (a -> Bool)
sneakyLambdaUnderDelay' = delay (let f _ =  adv (delay True) in f)

advUnderLambda :: O Int -> O (a -> Int)
advUnderLambda y = delay (\x -> adv y)

sneakyLambdaUnderDelay :: O (Int -> Int)
sneakyLambdaUnderDelay = delay (let f x = x in f)

-- This function is leaky unless the single tick transformation is
-- performed
leaky :: (() -> Bool) -> Str Bool
leaky p = p () ::: delay (leaky (\ _ -> hd (leaky (\ _ -> True))))

{-# ANN zeros ExpectWarning #-}
zeros :: Box (Str Int)
zeros = box (0 ::: delay (unbox zeros))

oneTwo :: Str Int
oneTwo = 1 ::: delay (2 ::: delay oneTwo)

data FStr a = Cons !a !(O (a -> O (FStr a)))

recFun :: Int -> FStr Int 
recFun n = Cons n (delay (\ x -> delay (recFun x)))

{-# ANN nestedRec ExpectWarning #-}
nestedRec :: Str Int
nestedRec = run 10
  where run :: Int -> Str Int
        run 0 = 0 ::: delay (nestedRec)
        run n = n ::: delay (run (n-1))
-}
{-# ANN main NotRattus #-}
main = putStrLn "This file should type check"
