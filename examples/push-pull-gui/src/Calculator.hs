{-# LANGUAGE OverloadedLists #-}{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import WidgetRattus.Behaviour
import WidgetRattus.Event
import WidgetRattus
import WidgetRattus.PushPull.Widgets
import Prelude hiding (zipWith, const)

nums :: List Int
nums = [0 .. 9]

data Op = Plus | Minus | Equals | Reset

compute :: (Int :* Op :* Bool -> (Int :* Op) -> Int :* Op :* Bool)
compute  _                 (_ :* Reset) = 0 :* Reset :* True
compute (n :* Plus   :* _) (m :* op)    = (n + m) :* op :* True
compute (n :* Minus  :* _) (m :* op)    = (n - m) :* op :* True
compute (_ :* Equals :* _) (m :* op)    = m :* op :* True
compute (_ :* Reset  :* _) (m :* op)    = m :* op :* True

calculatorExample :: C VStack'
calculatorExample = do
  -- Buttons
  numBtns :: List Button' <-
    mapM (mkButton' . const) nums

  let [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9] = numBtns
  resetBut <- mkButton' (mkConstText "C")
  addBut <- mkButton' (mkConstText "+")
  subBut <- mkButton' (mkConstText "-")
  eqBut <- mkButton' (mkConstText "=")

  -- Event to construct numbers
  let numClicks :: List (Ev (Int -> Int)) =
        zipWith' (\b n -> mapE (box (\_ x -> x * 10 + n)) (btnOnClickEv b)) numBtns nums

  -- Event to reset the current number to 0, after clicking an operator button
  let resetEv :: Ev (Int -> Int) =
        mapE (box (\_ _ -> 0)) $
          interleaveAll (box (\a _ -> a)) $
            map' btnOnClickEv [addBut, subBut, eqBut, resetBut]

  -- Combine events to construct the number event
  let evList = resetEv :! numClicks :: List (Ev (Int -> Int))
  let combinedEvs = interleaveAll (box (\a _ -> a)) evList

  -- Number event (i.e. the multidigit number that has been constructed)
  let numberEv :: Ev Int =
        scan (box (\a f -> f a)) 0 combinedEvs

  -- Operator event
  let opEv :: Ev Op =
          interleaveAll (box (\a _ -> a)) $
            map'
              (\(op :* btn) -> mapE (box (\_ -> op)) (btnOnClickEv btn))
              [Plus :* addBut, Minus :* subBut, Equals :* eqBut, Reset :* resetBut]

  -- Event consisting of an operand (i.e. a number) @n@ and an
  -- operator @op@. @n@ is the value of @numberEv@ just before
  -- clicking an operator button, and op is taken from opEv
  let operand :: Ev (Int :* Op) =
        trigger (box (\op n -> (n :* op))) opEv (stepper 0 (buffer 0 numberEv))

  -- The result event consisting of a number n that is the result
  -- of the current computation, an operator op that still needs to
  -- applied to n and a Boolean b that indicates whether we have
  -- just calculated n (and thus n should be displayed)
  let resultEv :: Ev (Int :* Op :* Bool) =
        scan (box compute) (0 :* Plus :* True) operand

  -- -- The behaviour that should be displayed
  let displayBeh :: Beh Int =
        zipWith (box (\(n :* _ :* b) m -> if b then n else m)) (stepper (0 :* Plus :* False) resultEv) (stepper 0 numberEv)

  -- UI
  result <- mkLabel' displayBeh
  operators <- mkConstVStack' (resetBut :* addBut :* subBut :* eqBut)
  row1 <- mkConstHStack' (b7 :* b8 :* b9)
  row2 <- mkConstHStack' (b4 :* b5 :* b6)
  row3 <- mkConstHStack' (b1 :* b2 :* b3)

  numbers <- mkConstVStack' (row1 :* row2 :* row3 :* b0)

  input <- mkConstHStack' (numbers :* operators)
  mkConstVStack' (result :* input)

main :: IO ()
main = runApplication' calculatorExample