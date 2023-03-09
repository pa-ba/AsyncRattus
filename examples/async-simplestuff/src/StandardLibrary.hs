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
mkChannel id f = Delay (Set.singleton id) f 

keyboard :: O Char
keyboard = mkChannel 1 (\(1, CharValue c) -> c)

reset :: O Bool
reset = mkChannel 2 (\(2, BoolValue b) -> b)


--------------------------------------

-- Streams based on input channels
kbStr :: O (Str Char)
kbStr = Delay cl ((\inputValue -> adv' keyboard inputValue ::: Delay cl (\inputValue2 -> adv' kbStr inputValue2)))
    where cl = extractClock keyboard

resetStr :: O (Str Bool)
resetStr = Delay cl ((\inputValue -> adv' reset inputValue ::: Delay cl (\inputValue2 -> adv' resetStr inputValue2)))
    where cl = extractClock reset
-----

-- Output Channel

accumulatorStr :: O (Str Char) -> O (Str String)
accumulatorStr str@(Delay cl inpF) = Delay cl (\inputValue@(_, CharValue value) -> scanAwait (box (\acc char -> acc ++ [char])) [value] str)

textStr :: O (Str String) -> O (Str Bool) -> O (Str String)
textStr accStr@(Delay clAcc inpFA) resetStr@(Delay clReset inpFR) = Delay (clAcc `Set.union` clReset) (\inputValue ->
        case sel inputValue of 
            Both a b -> error "Not happening atm."
            Left (a ::: as) b -> a ::: textStr as b 
            Right a (b ::: bs) -> "" ::: textStr (accumulatorStr kbStr) bs
        ) 
    where sel inp = select inp accStr resetStr
------


-- Transducer function

--textEditor :: Str InputValue -> Str String
--textEditor inputs =  
--    where text = textStr (accumulatorStr kbStr) resetStr 
----------------


-- New functions 

scan :: Box (b -> a -> b) -> b -> Str a -> Str b 
scan f acc (a ::: as) = acc' ::: Delay cl (\inputValue -> scan f acc' (adv' as inputValue))
    where acc' = unbox f acc a
          cl = extractClock as

scanAwait :: Box (b -> a -> b) -> b -> O (Str a) -> Str b
scanAwait f acc as = acc ::: Delay cl (\inputValue -> scan f acc (adv' as inputValue))
    where cl = extractClock as

select :: InputValue -> O a -> O b -> Select a b
select inputValue@(chId, value) a@(Delay clA inpFA) b@(Delay clB inpFB)  
  | chId `elem` clA && chId `elem` clB = Both (inpFA inputValue) (inpFB inputValue)
  | chId `elem` clA = Left (inpFA inputValue) b
  | chId `elem` clB = Right a (inpFB inputValue)
  | otherwise = error "Tick did not come on correct input channels"

----

mapL :: Box (a -> b) -> O a -> O b
mapL f (Delay cl inpF) = Delay cl (unbox f . inpF)

mapS :: Box (a -> b) -> Str a -> Str b
mapS f (a ::: as@(Delay cl inpF)) = unbox f a ::: Delay cl (\inp -> mapS f (adv' as inp))



