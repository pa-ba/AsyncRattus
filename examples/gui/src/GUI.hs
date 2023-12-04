{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}

module Main (module Main) where

import AsyncRattus
import AsyncRattus.Signal
import AsyncRattus.Channels
import AsyncRattus.Widgets

import Control.Concurrent ( forkIO )
import Control.Monad
import Prelude hiding (map, const, zipWith, zip, filter, getLine, putStrLn,null)
import Data.Text.IO
import Data.Text hiding (filter, map, all)
import qualified Data.Text as Text
import System.Exit
import Data.Text.Read
import Data.Char


isNumberText :: Text -> Bool
isNumberText = Text.all isDigit

-- Textfield that turns red if the text is not a natural number
mkNumberTf :: C TextField
mkNumberTf = do
    tf <- mkTextField (const "number") (const black)

    let sig = (mkSig (tfOnInput tf))
    
    let upd = mapAwait (box (\ inp -> if isNumberText inp then black else red)) sig
    return (ftSetColor tf upd)

-- Textfield that turns red if the text is not a natural number
mkNumberTf' :: Int -> C TextField
mkNumberTf' num = do
    tf <- mkTextField (const ("number " `append` pack (show num))) (const black)

    let sig = (mkSig (tfOnInput tf))
    
    let upd = mapAwait (box (\ inp -> if isNumberText inp then black else red)) sig
    return (ftSetColor tf upd)



-- What to do when the "New" button is clicked
updateWidgetGroup :: WidgetGroup -> C WidgetGroup
updateWidgetGroup old = do
    removeBtn <- mkButton (const "Remove Me") (const red)
    tf <- mkNumberTf
    let newBtn = endLater (Widget removeBtn) (unbox (btnOnClick removeBtn))
    let newTf = endLater (Widget tf) (unbox (btnOnClick removeBtn))
    return (constE (Widget (HStack (const [newTf, newBtn]))) :! old)

-- This describes future changes to the GUI
groupFuture :: Box (WidgetGroup) -> Box (O ()) -> C (O (Sig WidgetGroup))
groupFuture old click = delayC $ delay (do
            let () = adv (unbox click)
            new <- updateWidgetGroup (unbox old)
            fut <- groupFuture (promote new) click 
            return (new ::: fut))

-- This group describes the whole GUI
wgroup :: C (Sig WidgetGroup)
wgroup = do newBtn <-  mkButton (const "New") (const white)
            let grp =  [constE (Widget newBtn)]
            fut <- groupFuture (promote grp) (btnOnClick newBtn)
            return (grp ::: fut)

-- What to do when the "New" button is clicked
updateWidgetGroup' :: Int -> C (SigE Widget)
updateWidgetGroup' num = do
    removeBtn <- mkButton (const "Remove Me") (const red)
    tf <- mkNumberTf' num
    let newBtn = constE (Widget removeBtn) 
    let newTf = constE (Widget tf)
    return (endLater (Widget (HStack' (newTf `ConsU` (newBtn `ConsU` NilU)))) (unbox (btnOnClick removeBtn)))

-- This describes future changes to the GUI
listFuture :: Int -> Box (O ()) -> C (O (ListU Widget))
listFuture num click = delayC $ delay (do
            let () = adv (unbox click)
            new <- updateWidgetGroup' num
            fut <- listFuture (num + 1) click 
            return (new `ConsU` InsertU fut NilU))
-- This group describes the whole GUI
wlist :: C (ListU Widget)
wlist = do newBtn <-  mkButton (const "New") (const white)
           let grp =  constE (Widget newBtn)
           fut <- listFuture 1 (btnOnClick newBtn)
           return (grp `ConsU` (fut `InsertU` NilU))

-- alternative implementation of group using scanAwait
group' :: C (Sig WidgetGroup)
group' = do
    newBtn <- mkButton (const "New") (const white)
    let grp = [constE (Widget newBtn)]
        sig = mkSig (btnOnClick newBtn)
    scanAwaitC (box (\ w () -> updateWidgetGroup w)) grp sig

main = runApplication (VStack <$> group')
