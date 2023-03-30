module Main where

import StandardLibrary
import Rattus.ToHaskell
import Rattus.Primitives
import Graphics.Gloss.Interface.Pure.Game
import Rattus.Stream
import Debug.Trace

type World = (Maybe InputValue, String, O(Str String))

initial' :: World
initial' = (Nothing, "", textStr (accumulatorStr kbStr) resetStr)

render' :: World -> Picture
render' (_, text, _) = Translate (-(fromIntegral sizex)/2.0) 0 (Text text)

handleEvent' :: Event -> World -> World
handleEvent' (EventKey (Char c) Down _ _) (_, text, str) = (Just (1, CharValue c), trace ("This is the text now:" ++ text) text, str)
handleEvent' (EventKey (SpecialKey KeyF1) Down _ _) (_, text, str) = (Just (2, BoolValue True), trace ("This is a reset:" ++ text) text, str)
handleEvent' e world = trace ("nothing interesting happend: " ++ show e) world

step' :: Float -> World -> World
step' _ (Just input, _, laterStr) = (Nothing, a, as)
    where (a ::: as) =  adv' laterStr input 
step' _ world = world


-- Sizes
sizex = 1512 
sizey = 982 

main :: IO ()
main = play
       (InWindow "text writer 9000" (sizex, sizey) (0,0))
       white
       10
       initial'
       render'
       handleEvent'
       step'