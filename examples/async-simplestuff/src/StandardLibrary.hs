{-# LANGUAGE TypeOperators #-}

module Main where

import Rattus
import Rattus.Stream
import Rattus.ToHaskell
import Rattus.Primitives
import qualified Data.Set as Set

-- Input Channels

mkChannel :: Int -> (InputValue -> a) -> O a
mkChannel id f = Delay (Set.singleton id) f 

keyboard :: O Char
keyboard = mkChannel 1 (\(1, CharValue c) -> c)


--------------------------------------

-- Streams based on input channels
kbStr :: O (Str Char)
kbStr = Delay cl ((\inputValue -> adv' keyboard inputValue ::: Delay cl (\inputValue2 -> adv' kbStr inputValue2)))
    where cl = extractClock keyboard
-----




mapL :: Box (a -> b) -> O a -> O b
mapL f (Delay cl inpF) = Delay cl (unbox f . inpF)

mapS :: Box (a -> b) -> Str a -> Str b
mapS f (a ::: as@(Delay cl inpF)) = unbox f a ::: Delay cl (\inp -> mapS f (adv' as inp))

--switch :: Str a -> O (Str a) -> Str a
--switch (x ::: xs) delayed = 
--    x ::: delay' (case select xs d of
--                    Both  (a :* b)   -> b 
--                    Right (O a:* b)  -> b
--                    Left  (a :* O b) -> switch a b) 


main2 :: IO ()
main2 = do
    print (adv' laterB (1, IntValue 0))
    where laterA = delayCustom (Set.singleton 1) id
          laterInt = mapL (box (\ (chId, IntValue i) -> i)) laterA
          laterB = mapL (box (>1)) laterInt

{-main3 :: IO ()
main3 = do
    print (take 10 $ fromStr' str')
    where str' = mapS (box (*2)) (toStr' [1..])-}

main :: IO ()
main = do
    print (hd $ adv' str (1, CharValue 'h'))
    print (hd $ adv' str (1, CharValue 'e'))
    print (hd $ adv' str (1, CharValue 'j'))
    where str = kbStr