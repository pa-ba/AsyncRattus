{-# LANGUAGE TypeOperators #-}

module StandardLibrary where

import Rattus
import Rattus.Stream hiding (scan)
import Rattus.ToHaskell
import Rattus.Primitives
import Prelude hiding (Left, Right)
import qualified Data.Set as Set


-- Input Channels

mkChannel :: Int -> (InputValue -> a) -> O a
mkChannel id = Delay (Set.singleton id)

keyboard :: O Char
keyboard = mkChannel 1 (\(1, CharValue c) -> c)

reset :: O Bool
reset = mkChannel 2 (\(2, BoolValue b) -> b)


--------------------------------------

-- Streams based on input channels
kbStr :: O (Str Char)
kbStr = Delay cl (\inputValue -> adv' keyboard inputValue ::: Delay cl (adv' kbStr))
    where cl = extractClock keyboard

resetStr :: O (Str Bool)
resetStr = Delay cl (\inputValue -> adv' reset inputValue ::: Delay cl (adv' resetStr))
    where cl = extractClock reset
-----

-- Output Channel
accumulatorStr :: O (Str Char) -> O (Str String)
accumulatorStr str@(Delay cl inpF) = mapL (box (\(c ::: cs) -> (scanAwait (box (\acc char -> acc ++ [char])) [c] cs))) str

textStr :: O (Str String) -> O (Str Bool) -> O (Str String)
textStr accStr@(Delay clAcc inpFA) resetStr@(Delay clReset inpFR) = Delay (clAcc `Set.union` clReset) (\inputValue ->
        case sel inputValue of 
            Both a b -> error "Not happening atm."
            Left (a ::: as) b -> a ::: textStr as b 
            Right a (b ::: bs) -> "" ::: textStr (accumulatorStr kbStr) bs
        )
    where sel inp = select' accStr resetStr inp


-- Transducer function

--textEditor :: Str InputValue -> Str String
--textEditor inputs =  
--    where text = textStr (accumulatorStr kbStr) resetStr 
----------------


-- New functions 

scan :: Box (b -> a -> b) -> b -> Str a -> Str b
scan f acc (a ::: as) = acc' ::: Delay cl (scan f acc' . adv' as)
    where acc' = unbox f acc a
          cl = extractClock as

scanAwait :: Box (b -> a -> b) -> b -> O (Str a) -> Str b
scanAwait f acc as = acc ::: Delay cl (scan f acc . adv' as)
    where cl = extractClock as

----

mapL :: Box (a -> b) -> O a -> O b
mapL f (Delay cl inpF) = Delay cl (unbox f . inpF)

mapS :: Box (a -> b) -> Str a -> Str b
mapS f (a ::: as@(Delay cl inpF)) = unbox f a ::: Delay cl (mapS f . adv' as)

