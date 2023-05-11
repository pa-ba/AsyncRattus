{-# LANGUAGE TypeOperators #-}

module Cmd where 

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

command :: O (Stream Char) -> Stream (List Char)
command keyboardInput = Stream.scanAwait (box (\s c -> s +++ (c :! Nil))) Nil keyboardInput

commandLine' :: Stream (List Char) -> Box (O Input) -> Stream (List Char)
commandLine' (cmd ::: cmds) mouseClick = cmd ::: delay (
        case select cmds (unbox mouseClick) of
            Left cmds' _ -> commandLine' cmds' mouseClick
            Right cmds' _ -> commandLine' (command kb) mouseClick
            Both _ _ -> commandLine' (command kb) mouseClick
    )

commandLine :: Stream (List Char)
commandLine = commandLine' (command kb) mouse

-- Workaround for known bug which causes compiler panic when the Int implementation of the Ord typeclass is never used.
_ = Set.fromList [1]