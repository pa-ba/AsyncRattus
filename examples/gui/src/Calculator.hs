{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

import WidgetRattus
import WidgetRattus.Signal
import WidgetRattus.Widgets
import Prelude hiding (map, const, zipWith, zip, filter, getLine, putStrLn,null)
import Data.Text (Text)

nums :: List Int
nums = [0..9]

data Op = Plus | Minus | Equals | Reset

compute :: (Int :* Op :* Bool -> Maybe' (Int :* Op) -> Int :* Op :* Bool)
compute (n :* op     :* _) Nothing'          = (n :* op :* False)
compute _                  (Just' (_ :* Reset)) = (0 :* Reset :* True)
compute (n :* Plus   :* _) (Just' (m :* op)) = (n + m) :* op :* True
compute (n :* Minus  :* _) (Just' (m :* op)) = (n - m) :* op :* True
compute (_ :* Equals :* _) (Just' (m :* op)) = m :* op :* True
compute (_ :* Reset  :* _) (Just' (m :* op)) = m :* op :* True


window :: C VStack
window = do

    -- construct number buttons
    numBtns :: List Button  
        <- mapM (mkButton . const) nums
    let [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9] = numBtns
    -- construct operator buttons
    resetBut <- mkButton (const ("C"::Text))
    addBut   <- mkButton (const ("+"::Text))
    subBut   <- mkButton (const ("-"::Text))
    eqBut    <- mkButton (const ("="::Text))

    -- signal to construct numbers
    let numClicks :: List (O (Sig (Int -> Int))) 
          = zipWith' (\b n -> mapAwait (box (\ _ x -> x * 10 + n)) (btnOnClickSig b)) numBtns nums
          


    -- signal to reset the current number to 0, after clicking an
    -- operator button
    let resetSig :: O (Sig (Int -> Int))
          = mapAwait (box (\ _ _ -> 0))
            $ interleaveAll (box (\ a _ -> a))
            $ map' btnOnClickSig [addBut, subBut, eqBut,resetBut]

    -- combine signals to construct the number signal
    let sigList = resetSig :! numClicks :: List (O (Sig (Int -> Int)))
    let combinedSig = interleaveAll (box (\ a _ -> a)) sigList

    -- number signal (i.e. the multidigit number that has been
    -- constructed)
    let numberSig :: Sig Int
         = scanAwait (box (\ a f-> f a)) 0 combinedSig
    -- operator signal
    let opSig :: O (Sig Op)
         = interleaveAll (box (\ a _ -> a))
          $ map' (\ (op :* btn) -> mapAwait (box (\ _ -> op)) (btnOnClickSig btn) )
            [(Plus :* addBut), (Minus :* subBut), (Equals :* eqBut), (Reset :* resetBut)]

    -- signal consisting of an operand (i.e. a number) @n@ and an
    -- operator @op@. @n@ is the value of @numberSig@ just before
    -- clicking an operator button, and op is taken from opSig
    let operand :: Sig (Maybe' (Int :* Op))
         = Nothing' ::: triggerAwaitM (box (\op n -> Just' (n :* op))) opSig (buffer 0 numberSig)

    -- The result signal consisting of a number n that is the result
    -- of the current computation, an operator op that still needs to
    -- applied to n and a Boolean b that indicates whether we have
    -- just calculated n (and thus n should be displayed)
    let resSig :: Sig (Int :* Op :* Bool)
         = scan (box compute) (0 :* Plus :* True) operand
    -- The signal that should be displayed
    let displaySig :: Sig Int
         = zipWith (box (\ (n :* _ :* b) m -> if b then n else m)) resSig numberSig
    
    -- label to display the result (and operands)
    result <- mkLabel displaySig
    
    -- lay out widgets
    operators <- mkConstVStack (resetBut :* addBut :* subBut :* eqBut)
    row1 <- mkConstHStack (b7 :* b8 :* b9)
    row2 <- mkConstHStack (b4 :* b5 :* b6)
    row3 <- mkConstHStack (b1 :* b2 :* b3)

    numbers <- mkConstVStack (row1 :* row2 :* row3 :* b0)

    input <- mkConstHStack (numbers :* operators)

    mkConstVStack (result :* input)

main :: IO ()
main = runApplication window