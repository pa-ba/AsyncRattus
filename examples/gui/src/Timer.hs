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


everySecond :: Box (O())
everySecond = timer 1000000

everySecondSig :: Sig ()
everySecondSig = () ::: mkSig everySecond

nats :: (Int :* Int) -> Sig (Int :* Int)
nats (n :* max) = stop 
    (box (\ (n :* max) -> n >= max)) 
    (scanAwait (box (\ (n :* max) _ -> (min (n + 1) max) :* max)) (n :* max) (future everySecondSig))


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

    let counterSig :: Sig (Int :* Int)
         = switchB inputSig (box nats) (0 :* currentMax)
    
    let currentSig = map (box fst') counterSig
    let maxSig = map (box snd') counterSig

    label <- mkLabel (map (box display) currentSig)
    pb <- mkProgressBar (const 0) maxSig currentSig

    mkVStack (const [mkWidget slider,
                     mkWidget resetBtn,
                     mkWidget label,
                     mkWidget pb])

main :: IO ()
main = runApplication window