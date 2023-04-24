module Simple where

import Rattus
import Rattus.Stream (Str(..))
import qualified Rattus.Stream as Stream
import Rattus.Primitives
import qualified Data.Set as Set
import Prelude hiding (Left, Right, map, const)
import Rattus.Later (map)
import Rattus.Channels (mkChannels, InputFunc)

{-# ANN module Rattus #-}
data Test a = IntTest Int a

data MyValue = IntValue Int | BoolValue Bool | CharValue Char

--Constant
test :: Test Bool
test = IntTest 1 True

test2 :: Set.Set Int
test2 = Set.union (Set.singleton 1) (Set.singleton 2)

(input, [kbChannel, mouseChannel, numChannel]) = mkChannels ["keyboard", "mouse", "num"]

keyboard :: O MyValue Char
keyboard = map (\(CharValue c) -> c) kbChannel

describeChar :: Char -> String
describeChar c = "This keyboard input just arrived " ++ show c

describeKeyboard :: O MyValue String
describeKeyboard = map describeChar keyboard

numbers :: O MyValue Int
numbers = map (\(IntValue i) -> i) numChannel

numberStr :: O MyValue (Str MyValue Int)
numberStr = constLaterStr numbers

mappedStr :: O MyValue (Str MyValue Int)
mappedStr = map (Stream.map (box (+100))) $ constLaterStr numbers

scannedStr :: Str MyValue Int
scannedStr = Stream.scanAwait (box (+)) 0 numberStr

scanMappedStr :: O MyValue (Str MyValue Int)
scanMappedStr = 
    delay (
        Stream.scanMap (box (+)) (box (\x -> if even x then x else -x)) 0 (adv numberStr)
    )

-- should work
id3 :: O v a -> O v a
id3 a = delay (adv a)

-- should work
addOne :: O v Int -> O v Int
addOne li = delay (adv li + 22)

constLaterStr :: O v Int -> O v (Str v Int)
constLaterStr = Stream.fromLater

-- It is not _in general_ legal to advance on anything other than a var.
-- However here, because delay (adv x) = x, we want the whole thing to be
-- rewritten to an identity function.
-- should work
--id4 :: O Int -> O Int
--id4 x = delay (adv (delay (adv x)))

-- should work since we can extract the clock of the variable at runtime.
-- because of the toSingleTick pass, the result of the if-expression is bound to a new variable, so we cannot
-- distinguish between a legal variable and a synthesized one.

-- Right now, we are not ensured that clocks are compatible. This can lead to runtime errors. How to handle this?
-- The general case: if we bind one of several later values to a variable ie. in a case statement,
-- how do we know the clock at compile time?

-- possible solution: union clocks. This means that values may be recomputed unnecessarily. It would
-- still be better than Rattus.

-- should work
naiveIf :: Bool -> O v a -> O v a -> O v (Bool, a)
naiveIf b x y = delay (b, adv later)
    where
        later = case b of
            True -> x
            False -> x



-- should not work
naiveIf' :: Bool -> O v a -> O v a -> O v (Bool, a)
naiveIf' b x y = delay (b, adv (if b then x else y))
 
describe :: O v a -> O v b -> O v Int
describe a b = delay (case select a b of
            Both _ _ -> 1
            Left _ _ -> 2
            Right _ _ -> 3)


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
