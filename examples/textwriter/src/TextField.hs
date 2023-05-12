{-# LANGUAGE TypeOperators #-}

module TextField where 

import Prelude hiding (Left, Right)
import AsyncRattus (AsyncRattus(..), (|#|))
import qualified AsyncRattus.Channels as Channels
import AsyncRattus.Channels (mkChannels)
import qualified AsyncRattus.Primitives as Prim
import AsyncRattus.Primitives (box, unbox, select, delay, adv, Select(..), Box)
import qualified AsyncRattus.Stream as Stream
import AsyncRattus.Stream(Str(..))
import qualified AsyncRattus.Later as Later
import qualified AsyncRattus.Strict as Strict
import AsyncRattus.Strict (List(..), (+++))
import qualified Data.Set as Set

data Input = Kb !Char | Mouse
type O a = Prim.O Input a
type Stream a = Str Input a
type InputChannel = Channels.InputChannel Input

{-# ANN module AsyncRattus #-}

(input, inputMaybe, depend, (kbInp :! mouse :! Nil)) = mkChannels ("kb" :! "mouse" :! Nil)

kb :: O (Stream Char)
kb = Stream.mapL (box (\(Kb c) -> c)) (Stream.fromLater kbInp)

text :: O (Stream Char) -> Stream (List Char)
text = text' Nil

text' :: List Char -> O (Stream Char) -> Stream (List Char)
text' previous keyboardInput = previous ::: delay (
    let (char ::: chars) = adv keyboardInput
    in text' (previous +++ (Strict.singleton char)) chars
    )

resettableText' :: Stream (List Char) -> Box (O Input) -> Stream (List Char)
resettableText' (txt ::: txts) mouseClick = txt ::: delay (
        case select txts (unbox mouseClick) of
            Left txts' _ -> resettableText' txts' mouseClick
            Right txts' _ -> resettableText' (text kb) mouseClick
            Both _ _ -> resettableText' (text kb) mouseClick
    )

resettableText :: Stream (List Char)
resettableText = resettableText' (text kb) mouse

-- Workaround for known bug which causes compiler panic when the Int implementation of the Ord typeclass is never used.
_ = Set.fromList [1]