{-# LANGUAGE TypeOperators #-}
module Main where

import GHC.Float
import Behaviour
import Rattus.Yampa
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Color
import Rattus

noInput = Input False NoMove

type World = (Float,Float,Float,SF Input (Pos :* Float) ,Input)


initial :: World
initial = (0,0,0,pong, noInput)

render :: World -> Picture
render (x,y,pad,_,_) = 
  Pictures
  [ translate x y
    (Color white (ThickCircle 3 6))
    --(translate (-8) (0.5) (Color white (Scale 0.14 (-0.14) (Text "y"))))
  , (Color white (Polygon [(pad-20,-size_y/2+5),(pad+20,-size_y/2+5),
                           (pad+20,-size_y/2+10),(pad-20,-size_y/2+10)]))
  , Color white (Line [(-size_x/2,-size_y/2),(-size_x/2,size_y/2)
                      ,(size_x/2,size_y/2),(size_x/2,-size_y/2)])]

handleEvent :: Event -> World -> World
handleEvent e (x,y,pad,s,_) = (x,y,pad,s,inp)
  where inp =
          case e of
            (EventKey (SpecialKey KeySpace) Down _ _) ->
              Input {reset = True, move = NoMove}
            (EventKey (SpecialKey KeyLeft) Down _ _) ->
              Input {reset = False, move = StartLeft}
            (EventKey (SpecialKey KeyLeft) Up _ _) ->
              Input {reset = False, move = EndLeft}
            (EventKey (SpecialKey KeyRight) Down _ _) ->
              Input {reset = False, move = StartRight}
            (EventKey (SpecialKey KeyRight) Up _ _) ->
              Input {reset = False, move = EndRight}
            _ -> noInput

step :: Float -> World -> World
step f (_,_,_,st,b) = (x,y,pad, adv st',noInput)
  where (st':* ((x:*y):*pad)) = stepSF st (float2Double(4*f)) b 
  



main :: IO ()
main = play
       (InWindow "bouncy lambda" (size_x',size_y') (100,100))
       black
       60
       initial
       render
       handleEvent
       step
