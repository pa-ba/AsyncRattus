{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

import WidgetRattus
import WidgetRattus.Signal
import WidgetRattus.Widgets
import Prelude hiding (map, const, zipWith, zip, filter, getLine, putStrLn,null)
import Data.Text (Text)

nums :: List Int
nums = [0..9]

window :: C VStack
window = do
    numBtns :: List Button  
        <- mapM (mkButton . const) nums

    let numClicks :: List (O (Sig (Int -> Int))) 
          = zipWith' (\b n -> mapAwait (box (\ _ x -> x * 10 + n)) (btnOnClickSig b)) numBtns nums
          
    let [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9] = numBtns

    addBut <- mkButton (const ("+"::Text))
    subBut <- mkButton (const ("-"::Text))
    eqBut <- mkButton (const ("="::Text))

    let resetSig =
            mapAwait (box (\ _ _ -> 0))
            (interleave (box (\ a _ -> a)) (interleave (box (\ a _ -> a)) (btnOnClickSig addBut) (btnOnClickSig subBut)) (btnOnClickSig eqBut))

    let sigList = resetSig :! numClicks :: List (O (Sig (Int->Int)))
    let combinedSig = foldl1 (interleave (box (\ a _ -> a))) sigList

    let numberSig = scanAwait (box (\ a f-> f a)) 0 combinedSig
    let bufferedSig = buffer 0 numberSig

    let addSig = mapAwait (box (\ _ -> box (+))) (btnOnClickSig addBut)
    let subSig = mapAwait (box (\ _ -> box (-))) (btnOnClickSig subBut)
    let opSig = interleave (box (\ a _ -> a)) addSig subSig

    let calcSig = triggerStable (box (\ op x -> box (unbox op x))) (box (0 +)) opSig bufferedSig

    let resultSig = zipWith (box (\ f x -> unbox f x)) calcSig bufferedSig
  
    let eqSig = triggerStable (box (\ _ x -> x)) 0 (btnOnClickSig eqBut) resultSig

   

    let displaySig = 0 ::: interleave (box (\ _ b -> b)) (future numberSig) (future eqSig)
    

    result <- mkLabel displaySig

    operators <- mkVStack (const [addBut, subBut, eqBut])
    row1 <- mkHStack (const [b7, b8, b9])
    row2 <- mkHStack (const [b4, b5, b6])
    row3 <- mkHStack (const [b1, b2, b3])

    numbers <- mkVStack (const [mkWidget row1, mkWidget row2 , mkWidget row3,  mkWidget b0])

    input <- mkHStack (const [mkWidget numbers, mkWidget operators])

    mkVStack (const [mkWidget result, mkWidget input])

main :: IO ()
main = runApplication window