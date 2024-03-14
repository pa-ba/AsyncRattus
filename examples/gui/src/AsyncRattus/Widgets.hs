
{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}


module AsyncRattus.Widgets where

import AsyncRattus
import AsyncRattus.Signal
import AsyncRattus.Channels
import Data.Text
import AsyncRattus.InternalPrimitives
import System.IO.Unsafe
import Control.Concurrent hiding (Chan)
import Data.IntSet as IntSet
import Prelude as Prelude
import qualified Monomer

data SigE a = !a ::* !(O (SigE a)) | SigEnd

instance Continuous a => Continuous (SigE a) where
    progressInternal _ SigEnd = SigEnd
    progressInternal inp (x ::* xs@(Delay cl _)) = 
        if inputInClock inp cl then adv' xs inp
        else progressInternal inp x ::* xs
    progressAndNext _ SigEnd = (SigEnd, emptyClock)
    progressAndNext inp (x ::* xs@(Delay cl _)) = 
        if inputInClock inp cl then let n = adv' xs inp in (n, nextProgress n)
        else let (n , cl') = progressAndNext inp x in (n ::* xs , cl `clockUnion` cl')
    nextProgress SigEnd = emptyClock
    nextProgress (x ::* (Delay cl _)) = nextProgress x `clockUnion` cl


-- | Construct a constant signal that never updates.
constE :: a -> SigE a
constE x = x ::* never

-- Singleton signal that ends upon a given tick
endLater :: a -> O () -> SigE a
endLater x click = x ::* (delay (adv click `seq` SigEnd))

data AppModel where
    AppModel :: IsWidget a => !a -> !Clock -> AppModel

instance (Eq AppModel) where
    _ == _ = False

data AppEvent where
    AppEvent :: !(Chan a) -> !a -> AppEvent

class Continuous a => IsWidget a where
    mkWidget :: a -> Monomer.WidgetNode AppModel AppEvent

instance IsWidget a => IsWidget (Sig a) where
    mkWidget (w ::: _) = mkWidget w


data Color = Color {redc :: !Float, greenc :: !Float, bluec :: !Float}

data TextField = TextField {tfText :: !(Sig Text), tfInput :: !(Chan Text) , tfColor :: !(Sig Color)}


data Button = Button {btnText :: !(Sig Text), btnClick :: !(Chan ()) , btnColor :: !(Sig Color)}

data Widget where
    Widget :: IsWidget a => !a -> Widget

continuous ''Button
continuous ''TextField
continuous ''Widget


instance IsWidget TextField where
    mkWidget TextField{tfText = txt ::: _, tfInput = inp, tfColor = color ::: _ } 
        = Monomer.textFieldV txt (\ t -> AppEvent inp t)

{-# ANN module AllowLazyData #-}
instance IsWidget Button where
    mkWidget Button{btnText = txt ::: _ , btnClick = click, btnColor = color ::: _ } 
        = Monomer.button txt (AppEvent click ())
    
instance IsWidget Widget where
    mkWidget (Widget w) = mkWidget w

type WidgetGroup = List (SigE Widget)

data HStack = HStack !(Sig WidgetGroup)
data VStack = VStack !(Sig WidgetGroup)


continuous ''HStack
continuous ''VStack

{-# ANN mkWidget' AllowRecursion #-}
mkWidget' :: List (SigE Widget) -> List (Monomer.WidgetNode AppModel AppEvent)
mkWidget' Nil = Nil
mkWidget' (SigEnd :! ws) = mkWidget' ws
mkWidget' ((w ::* _) :! ws) = mkWidget w :! mkWidget' ws

instance IsWidget HStack where
    mkWidget (HStack (ws ::: _)) = Monomer.hstack (mkWidget' ws)
instance IsWidget VStack where
    mkWidget (VStack (ws ::: _)) = Monomer.vstack (mkWidget' ws)


mkButton :: Sig Text -> Sig Color -> C Button
mkButton t c  = do ch <- chan
                   return (Button {btnClick = ch, btnText = t, btnColor = c})

mkTextField :: Sig Text -> Sig Color -> C TextField
mkTextField t c = do ch <- chan
                     return (TextField {tfInput = ch, tfText = t, tfColor = c})


mkTextField' :: Text -> Sig Color -> C TextField
mkTextField' t c = do ch <- chan
                      return (TextField {tfInput = ch, tfText = t ::: mkSig (box (wait ch)), tfColor = c})

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

mkTimerEvent :: Int -> (AppEvent -> IO ()) -> IO ()
mkTimerEvent n cb = (threadDelay n >> cb (AppEvent (Chan n) ())) >> return ()

runApplication :: IsWidget a => C a -> IO ()
runApplication (C w) = do
    w' <- w
    let cl = nextProgress w'
    Monomer.startApp (AppModel w' emptyClock) handler builder config
    where builder _ (AppModel w _) = mkWidget w
          handler _ _ (AppModel w cl) (AppEvent (Chan ch) d) = 
            let inp = (OneInput ch d) in unsafePerformIO $ do
               progressPromoteStoreAtomic inp
               let (w' , cl') = progressAndNext inp w
               let activeTimers = if ch > 0 then IntSet.delete ch cl else cl
               let newTimers = IntSet.filter (> 0) cl' `IntSet.difference` activeTimers
               let timers = Prelude.map (Monomer.Producer . mkTimerEvent) (IntSet.toList newTimers)
               return (Monomer.Model (AppModel w' (newTimers `IntSet.union` activeTimers)) : Monomer.Request Monomer.RenderOnce : timers )
          config = [
                Monomer.appWindowTitle "Test",
                Monomer.appTheme Monomer.lightTheme,
                Monomer.appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
                Monomer.appInitEvent (AppEvent (Chan 1) ())
                ]