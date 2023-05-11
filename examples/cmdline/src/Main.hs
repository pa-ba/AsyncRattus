module Main where

import AsyncRattus.Strict
import AsyncRattus.Stream (Str(..), tl, hd)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Graphics.Gloss.Interface.Pure.Game
import Cmd


type World = (Maybe (String, Input), Str Input (List Char))

initial' :: World
initial' = (Nothing, commandLine)

render' :: World -> Picture
render' (_, str) = Translate (-(fromIntegral sizex)/2.0) 0 (Text (toList (hd str)))

handleEvent' :: Event -> World -> World
handleEvent' (EventKey (Char c) Down _ _) (_, s) = (Just ("kb", Kb c), s)
handleEvent' (EventKey (MouseButton LeftButton) Down _ _) (_, s) = (Just ("mouse", Mouse True), s)
handleEvent' (EventKey (MouseButton RightButton) Down _ _) (_, s) = (Just ("mouse", Mouse False), s)
handleEvent' e world = world

step' :: Float -> World -> World
step' _ (Just (ch, val), str) = (Nothing, str')
    where str' = fromMaybe str $ inputMaybe ch val (tl str)
step' _ world = world


-- Sizes
sizex = 1512 
sizey = 982 


main = play
       (InWindow "text writer 9000" (sizex, sizey) (0,0))
       white
       10
       initial'
       render'
       handleEvent'
       step'
