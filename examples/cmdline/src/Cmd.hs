{-# LANGUAGE TypeOperators #-}

module Cmd where 

import Prelude hiding (Left, Right)
import AsyncRattus (AsyncRattus(..))
import qualified AsyncRattus.Channels as Channels
import AsyncRattus.Channels (mkChannels)
import qualified AsyncRattus.Primitives as Prim
import AsyncRattus.Primitives (box, unbox, select, delay, adv, Select(..))
import qualified AsyncRattus.Stream as Stream
import AsyncRattus.Stream(Str(..))
import qualified AsyncRattus.Strict as Strict
import AsyncRattus.Strict ((+++))
import AsyncRattus.Strict (List(..))
import qualified Data.Set as Set

data Input = Kb !Char | Mouse !Bool
type O a = Prim.O Input a
type Stream a = Str Input a
type InputChannel = Channels.InputChannel Input

{-# ANN module AsyncRattus #-}

(input, inputMaybe, depend, channels) = mkChannels ("kb" :! "mouse" :! Nil)

(kbI :! mouseI :! Nil) = Strict.map' (Stream.fromLater) channels

kb :: O (Stream Char)
kb = Stream.mapL (box (\(Kb c) -> c)) kbI

mouse :: O (Stream Bool)
mouse = Stream.mapL (box (\(Mouse b) -> b)) mouseI

command :: O (Stream Char) -> Stream (List Char)
command keyboardInput = Stream.scanAwait (box (\s c -> s +++ (c :! Nil))) Nil keyboardInput

commandLine' :: Stream (List Char) -> O (Stream Bool) -> Stream (List Char)
commandLine' (cmd ::: cmds) mouseInput = cmd ::: delay (
        case select cmds mouseInput of
            Left cmds' mouseInput' -> commandLine' cmds' mouseInput'
            Right cmds' (isLeftClick ::: mouseInput') | isLeftClick -> commandLine' (command kb) mouseInput'
            Right cmds' (_ ::: mouseInput') -> commandLine' (cmd ::: cmds') (mouseInput')
            Both _ (isLeftClick ::: mouseInput') | isLeftClick -> commandLine' (command kb) mouseInput'
            Both cmds' (_ ::: mouseInput') -> commandLine' cmds' mouseInput'
    )

commandLine :: Stream (List Char)
commandLine = commandLine' (command kb) mouse

-- Workaround for known bug which causes compiler panic when the Int implementation of the Ord typeclass is never used.
_ = Set.fromList [1]