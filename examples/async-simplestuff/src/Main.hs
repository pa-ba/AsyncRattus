module Main where

import StandardLibrary
import Rattus.ToHaskell
import Rattus.Primitives
import Graphics.Gloss.Interface.Pure.Game
import Rattus.Stream
import Debug.Trace

type State = (Maybe InputValue, String, Trans InputValue String) 

initial :: State
initial = (Nothing, "", runInputTransducer $ textEditor textStr')  

render :: State -> Picture
render (_, text, _) = Translate (-fromIntegral sizex/2.0) 0 (Text text)

handleEvent :: Event -> State -> State
handleEvent (EventKey (Char c) Down _ _) (_, text, trans) = (Just (1, CharValue c), text, trans)
handleEvent (EventKey (SpecialKey KeyF1) Down _ _) (_, text, trans) = (Just (2, BoolValue True), text, trans)
handleEvent _ state = state

step :: Float -> State -> State
step _ (Just input, _, Trans st) = (Nothing, text, st')
    where (text, st') = st input
step _ state = state

-- Sizes
sizex = 1512 
sizey = 982 

main :: IO ()
main = play
       (InWindow "text writer 9000" (sizex, sizey) (0,0))
       white
       10
       initial
       render
       handleEvent
       step