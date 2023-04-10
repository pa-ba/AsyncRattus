module Simple where

import Rattus
import Rattus.Stream (Str(..))
import Rattus.ToHaskell
import Rattus.Primitives
import qualified Data.Set as Set
import Prelude hiding (Left, Right)
import Debug.Trace as D

{-# ANN module Rattus #-}
data Test a = IntTest Int a
type Input2Test = (Int, Value)

--Constant
test :: Test Bool
test = IntTest 1 True

-- should work
id3 :: O a -> O a
id3 a = delay (adv a)

-- should work
addOne :: O Int -> O Int
addOne li = delay (adv li + 22)

-- It is not _in general_ legal to advance on anything other than a var.
-- However here, because delay (adv x) = x, we want the whole thing to be
-- rewritten to an identity function.
-- should work
--id4 :: O Int -> O Int
--id4 x = delay (adv (delay (adv x)))

-- should not work since we cannot guarantee that the clocks of x and y are compatible.
-- however, the compiler synthesizes a fresh variable with the result of the if statement, so we cannot
-- distinguish between a legal variable and a synthesized one.
-- the general case: if we bind one of several later values to a variable ie. in a case statement,
-- how do we know the clock at compile time?

-- possible solution: union clocks. This means that values may be recomputed unnecessarily. It would
-- still be better than Rattus.

-- should maybe work?
--naiveIf :: Bool -> O a -> O a -> O (Bool, a)
--naiveIf b x y = delay (b, adv (if b then x else y))

{- 
-- Fungerer ikke endnu, fordi select-primitiven ikke er implementeret.
-- Skal komme til at fungere
describe :: O a -> O b -> O String
describe a b = delay (case select a b of
            Both _ _ -> "Both"
            Left _ _ -> "Left"
            Right _ _ -> "Right")
-}

-- invalid. We do not support nested delays
--stutter :: Int -> Str Int
--stutter n = n ::: delay (n ::: delay (stutter (n+1)))

-- invalid. Only a single adv can appear inside a delay
--add :: O Int -> O Int -> O Int
--add x y = delay (adv x + adv y)

    {-
    case select a b of
        Left _ _ -> "Left"
        Right _ _ -> "Right"
        Both _ _ -> "Both"
-}
--------------------------------------
