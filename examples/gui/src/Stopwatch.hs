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

sampleInterval :: O ()
sampleInterval = timer 20000

currentTime :: C (Sig Time)
currentTime = do
     t <- time 
     return (t ::: mkSig (box (delayC $ delay (let _ = adv sampleInterval in time))))
     



elapsedTime :: C (NominalDiffTime -> Sig NominalDiffTime)
elapsedTime =  do t <- time
                  return (\ s -> run s t)
     where run :: NominalDiffTime -> Time -> Sig NominalDiffTime
           run start t = 
               start ::: delayC (delay (
                    let _ = adv sampleInterval 
                    in do t' <- time
                          return (run (start + (t' `diffTime` t)) t')))

window :: C VStack
window = do
    startBtn <- mkButton (const ("Start" :: Text))
    stopBtn <- mkButton (const ("Stop" :: Text))
    let startDelay = btnOnClick startBtn
    let startSig :: O (Sig (NominalDiffTime -> Sig NominalDiffTime)) 
         = mkSig' (box (delay (let _ = adv (unbox startDelay) in elapsedTime)))

    let stopDelay = btnOnClick stopBtn
    let stopSig :: O (Sig (NominalDiffTime -> Sig NominalDiffTime)) 
         = mkSig (box (delay (let _ = adv (unbox stopDelay) in const)))

    
    let inputSig :: O (Sig (NominalDiffTime -> Sig NominalDiffTime))
         = interleave (box (\ x _ -> x)) startSig stopSig


    let stopWatchSig :: Sig NominalDiffTime
         = switchR (const 0) inputSig

    timeLabName <- mkLabel (const ("Current Time:" :: Text))
    swLabName <- mkLabel (const ("Elapsed Time:" :: Text))


    timeLab <- currentTime >>= mkLabel
    stopWatchLab <- mkLabel stopWatchSig
    buttons <- mkConstHStack (startBtn :* stopBtn)
    time <- mkConstHStack (timeLabName :* timeLab)
    sw <- mkConstHStack (swLabName :* stopWatchLab)

    mkConstVStack (time :* sw :* buttons)

main :: IO ()
main = runApplication window