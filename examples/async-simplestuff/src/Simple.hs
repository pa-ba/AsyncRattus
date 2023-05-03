module Simple where

import Rattus (Rattus(..))
import Rattus.Stream (Str(..))
import qualified Rattus.Stream as Stream
import qualified Rattus.Primitives as Prim
import Rattus.Primitives (delay, adv, select, box, unbox, Select(..))
import qualified Data.Set as Set
import Prelude hiding (Left, Right, map, const)
import Rattus.Later (map)
import Rattus.Channels (mkChannels, InputFunc)

{-# ANN module Rattus #-}
data Test a = IntTest Int a

data MyValue = IntValue Int | BoolValue Bool | CharValue Char

type O a = Prim.O MyValue a
type Stream a = Str MyValue a

--Constant
test :: Test Bool
test = IntTest 1 True

test2 :: Set.Set Int
test2 = Set.union (Set.singleton 1) (Set.singleton 2)

(input, inputMaybe, depend, [kbChannel, mouseChannel, numChannel, num2Channel, num3Channel]) = mkChannels ["keyboard", "mouse", "num", "num2", "num3"]

keyboard :: O Char
keyboard = map (box toChar) kbChannel

describeChar :: Char -> String
describeChar c = "This keyboard input just arrived " ++ show c

describeKeyboard :: O String
describeKeyboard = map (box describeChar) keyboard

toChar :: MyValue -> Char
toChar (CharValue c) = c

toInt :: MyValue -> Int
toInt (IntValue i) = i

num :: O Int
num = map (box toInt) numChannel

num2 :: O Int
num2 = map (box toInt) num2Channel

num3 :: O Int
num3 = map (box toInt) num3Channel

numbers :: O Int
numbers = map (box toInt) numChannel

numberStr :: O (Stream Int)
numberStr = constLaterStr numbers

mappedStr :: O (Stream Int)
mappedStr = delay (Stream.map (box (+100)) (adv (constLaterStr numbers)))

scannedStr :: Stream Int
scannedStr = Stream.scanAwait (box (+)) 0 numberStr

scanMappedStr :: O (Stream Int)
scanMappedStr = 
    delay (
        Stream.scanMap (box (+)) (box (\x -> if even x then x else -x)) 0 (adv numberStr)
    )

-- should work
id3 :: O a -> O a
id3 a = delay (adv a)

-- should work
add :: Int -> O Int -> O Int
add k li = delay (adv li + k)

constLaterStr :: O Int -> O (Stream Int)
constLaterStr = Stream.fromLater

-- It is not _in general_ legal to advance on anything other than a var.
-- However here, because delay (adv x) = x, we want the whole thing to be
-- rewritten to an identity function.
-- should work
--id4 :: O Int -> O Int
--id4 x = delay (adv (delay (adv x)))

 
describe :: O a -> O b -> O String
describe a b = delay (case select a b of
            Both _ _ -> "Both"
            Left _ _ -> "Left"
            Right _ _ -> "Right")



constIf0 :: Int -> O Int -> O Int
constIf0 i later =
    delay (
        case i of
            0 -> 47
            1 -> 48
            _ -> adv later
    )

const47Later = constIf0 0 num
const48Later = constIf0 1 num
const49Later = constIf0 2 num2
const50Later = constIf0 2 num3
idLater = constIf0 2 num

{-
naiveSwitch :: Int -> O v Int -> O v Int -> O v Int
naiveSwitch i later1 later2 =
    delay (
        case i of
            0 -> adv later1
            _ -> adv later2
    )

naiveSwitch1 = naiveSwitch 0 num num2
naiveSwitch2 = naiveSwitch 1 num num2
-}

funkyExample :: Int -> O Int -> O Int
funkyExample n later =
    delay (
        case n of
            0 -> adv later
            _ -> 
                case select later later of
                    Left a _ -> a
                    Right _ b -> b
                    Both a b -> a + b
    )

myFunkyExample :: O Int
myFunkyExample = funkyExample 0 num

myFunkyExample2 :: O Int
myFunkyExample2 = funkyExample 1 num


-- invalid. We do not support nested delays
--stutter :: Int -> Stream Int
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

-- should not work, and doesn't
-- tomorrowPlusOne :: O Int -> Int
-- tomorrowPlusOne laterI = adv laterI + 1

-- nestedDelay :: O Int -> O (O Int)
-- nestedDelay k = delay (delay (adv k + 1))
