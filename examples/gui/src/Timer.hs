{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}


import WidgetRattus
import WidgetRattus.Signal
import WidgetRattus.Widgets
import Prelude hiding (map, const, zipWith, zip, filter, getLine, putStrLn,null)
import Data.Text hiding (filter, map, all)


everySecondSig :: O (Sig ())
everySecondSig = mkSig (box (timer 1000000))

nats :: (Int :* Int) -> Sig (Int :* Int)
nats (n :* max) = stop 
    (box (\ (n :* max) -> n >= max)) 
    (scanAwait (box (\ (n :* max) _ -> (n + 1) :* max)) (n :* max) everySecondSig)


reset :: (Int :* Int) -> (Int :* Int)
reset (_ :* max) = (0 :* max)

setMax :: Int -> (Int :* Int) -> (Int :* Int)
setMax max' (n :* _) = ((min n max') :* max')

window :: C VStack
window = do
    slider <- mkSlider 50 (const 1) (const 100)
    resetBtn <- mkButton (const ("Reset" :: Text))

    let resSig :: O (Sig ()) 
         = mkSig (btnOnClick resetBtn)
    let resetSig :: O (Sig (Int :* Int -> Int :* Int))
         = mapAwait (box (\ _ -> reset)) resSig

    let currentMax :: Int
         = current (sldCurr slider)
    let setMaxSig :: O (Sig (Int :* Int -> Int :* Int)) 
         = mapAwait (box setMax) (future (sldCurr slider))
 
    let inputSig :: O (Sig (Int :* Int -> Int :* Int))
         = interleave (box (.)) resetSig setMaxSig

    let inputSig' :: O (Sig (Int :* Int -> Sig (Int :* Int)))
         = mapAwait (box (nats .)) inputSig

    let counterSig :: Sig (Int :* Int)
         = switchR (nats (0 :* currentMax)) inputSig'
    
    let currentSig = map (box fst') counterSig
    let maxSig = map (box snd') counterSig

    label <- mkLabel currentSig
    pb <- mkProgressBar (const 0) maxSig currentSig

    mkConstVStack (slider :* resetBtn :* label :* pb)

main :: IO ()
main = runApplication window