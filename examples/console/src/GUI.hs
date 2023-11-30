{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}

module Main (module Main) where

import AsyncRattus
import AsyncRattus.Signal
import AsyncRattus.Channels

import Control.Concurrent ( forkIO )
import Control.Monad
import Prelude hiding (map, const, zipWith, zip, filter, getLine, putStrLn,null)
import Data.Text.IO
import Data.Text hiding (filter, map, all)
import qualified Data.Text as Text
import System.Exit
import Data.Text.Read
import Data.Char

import AsyncRattus.InternalPrimitives

data SigE a = !a ::* !(O (SigE a)) | SigEnd

instance Continuous a => Continuous (SigE a) where
    progressInternal _ SigEnd = SigEnd
    progressInternal inp@(InputValue chId _) (x ::* xs@(Delay cl _)) = 
        if channelMember chId cl then adv' xs inp
        else progressInternal inp x ::* xs


-- | Construct a constant signal that never updates.
constE :: a -> SigE a
constE x = x ::* never

-- Singleton signal that ends upon a given tick
endLater :: a -> O () -> SigE a
endLater x click = x ::* (delay (adv click `seq` SigEnd))


data Color = Color {redc :: !Float, greenc :: !Float, bluec :: !Float}
data TextField = TextField {tfText :: !(Sig Text), tfInput :: !(Chan Text) , tfColor :: !(Sig Color)}
data Button = Button {btnText :: !(Sig Text), btnClick :: !(Chan ()) , btnColor :: !(Sig Color)}

data Widget where
    Widget :: Continuous a => !a -> Widget

continuous ''Button
continuous ''TextField
continuous ''Widget


type WidgetGroup = List (SigE Widget)

mkButton :: Sig Text -> Sig Color -> Button
mkButton t c  = Button {btnClick = chan, btnText = t, btnColor = c}

mkTextField :: Sig Text -> Sig Color -> TextField
mkTextField t c  = TextField {tfInput = chan, tfText = t, tfColor = c}

btnOnClick :: Button -> Box (O ())
btnOnClick btn = let ch = btnClick btn
                 in box (wait ch)
tfOnInput :: TextField -> Box (O Text)
tfOnInput tf = let ch = tfInput tf
               in box (wait ch)

ftSetColor :: TextField -> O (Sig Color) -> TextField
ftSetColor tf upd = tf {tfColor = switch (tfColor tf) upd}



red :: Color
red = Color {redc = 1, greenc = 0, bluec = 0}

white :: Color
white = Color {redc = 1, greenc = 1, bluec = 1}

black :: Color
black = Color {redc = 0, greenc = 0, bluec = 0}

isNumberText :: Text -> Bool
isNumberText = Text.all isDigit

-- Textfield that turns red if the text is not a natural number
mkNumberTf :: TextField
mkNumberTf = ftSetColor tf upd where 
    tf = mkTextField (const "") (const black)
    sig :: O (Sig Text)
    sig = (mkSig (tfOnInput tf))
    upd :: O (Sig Color)
    upd = mapAwait (box (\ inp -> if isNumberText inp then black else red)) sig


-- What to do when the "New" button is clicked
updateWidgetGroup :: WidgetGroup -> WidgetGroup
updateWidgetGroup old = new where
    removeBtn = mkButton (const "Remove Me") (const red)
    tf = mkNumberTf
    newBtn = endLater (Widget removeBtn) (unbox (btnOnClick removeBtn))
    newTf = endLater (Widget tf) (unbox (btnOnClick removeBtn))
    new = (newTf :! newBtn :! old)

-- This describes future changes to the GUI
groupFuture :: WidgetGroup -> Box (O ()) -> O (Sig WidgetGroup)
groupFuture old click = delay (
            let () = adv (unbox click)
                new = updateWidgetGroup (progress old)
            in new ::: groupFuture new click )

-- This group describes the whole GUI
group :: Sig WidgetGroup
group =  let newBtn = mkButton (const "New") (const white)
             grp =  [constE (Widget newBtn)]
         in grp ::: groupFuture grp (btnOnClick newBtn)


-- alternative implementation of group using switchS
group' :: Sig WidgetGroup
group' = scanAwait (box (\ w () -> updateWidgetGroup w)) grp sig where 
    newBtn = mkButton (const "New") (const white)
    grp = [constE (Widget newBtn)]
    sig :: O (Sig ())
    sig = mkSig (btnOnClick newBtn)
    

main = startEventLoop
