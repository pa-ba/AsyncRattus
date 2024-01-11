
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

import qualified Monomer

data SigE a = !a ::* !(O (SigE a)) | SigEnd

instance Continuous a => Continuous (SigE a) where
    progressInternal _ SigEnd = SigEnd
    progressInternal inp@(InputValue chId _) (x ::* xs@(Delay cl _)) = 
        if channelMember chId cl then adv' xs inp
        else progressInternal inp x ::* xs


data ListU a = ConsU !(SigE a) !(ListU a) | InsertU !(O (ListU a)) !(ListU a) | NilU

{-# ANN appendU AllowRecursion #-}
appendU :: ListU a -> ListU a -> ListU a
appendU NilU ys = ys
appendU (ConsU x xs) ys = ConsU x (appendU xs ys)
appendU (InsertU d xs) ys = InsertU d (appendU xs ys)

instance Continuous a => Continuous (ListU a) where
    progressInternal _ NilU = NilU
    progressInternal inp (ConsU SigEnd xs) = (progressInternal inp xs)
    progressInternal inp@(InputValue chId _) (ConsU x@(_ ::* xs'@(Delay cl _)) xs) = 
        if channelMember chId cl then ConsU (adv' xs' inp) xs
            else ConsU (progressInternal inp x) (progressInternal inp xs)
    progressInternal inp@(InputValue chId _) (InsertU xs@(Delay cl _) ys) = 
        if channelMember chId cl then appendU (adv' xs inp) ys
        else  InsertU xs (progressInternal inp ys)


-- | Construct a constant signal that never updates.
constE :: a -> SigE a
constE x = x ::* never

-- Singleton signal that ends upon a given tick
endLater :: a -> O () -> SigE a
endLater x click = x ::* (delay (adv click `seq` SigEnd))

data AppModel where
    AppModel :: IsWidget a => !a -> AppModel

instance (Eq AppModel) where
    _ == _ = False

data AppEvent where
    AppEvent :: !(Chan a) -> !a -> AppEvent

class Continuous a => IsWidget a where
    mkWidget :: a -> Monomer.WidgetNode AppModel AppEvent

instance IsWidget a => IsWidget (Sig a) where
    mkWidget (w ::: _) = mkWidget w

-- instance IsWidget a => IsWidget (SigE a) where
--     mkWidget (w ::* _) = mkWidget w

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

{-# ANN mkWidgetList AllowRecursion #-}
mkWidgetList :: ListU Widget -> List (Monomer.WidgetNode AppModel AppEvent)
mkWidgetList NilU = Nil
mkWidgetList (ConsU SigEnd xs) = mkWidgetList xs
mkWidgetList (ConsU (x ::* _) xs) = mkWidget x :! mkWidgetList xs
mkWidgetList (InsertU _ xs) = mkWidgetList xs


data HStack' = HStack' !(ListU Widget)
data VStack' = VStack' !(ListU Widget)

continuous ''HStack'
continuous ''VStack'

instance IsWidget HStack' where
    mkWidget (HStack' ws) = Monomer.hstack (mkWidgetList ws)
instance IsWidget VStack' where
    mkWidget (VStack' ws) = Monomer.vstack (mkWidgetList ws)

mkButton :: Sig Text -> Sig Color -> C Button
mkButton t c  = do ch <- chan
                   return (Button {btnClick = ch, btnText = t, btnColor = c})

mkTextField :: Sig Text -> Sig Color -> C TextField
mkTextField t c = do ch <- chan
                     return (TextField {tfInput = ch, tfText = t, tfColor = c})

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


runApplication :: IsWidget a => C a -> IO ()
runApplication (C w) = do
    forkIO startEventLoop -- TODO: Can we run this rather as part of the Monomer event loop?
    w' <- w
    Monomer.startApp (AppModel w') handler builder config
    where builder _ (AppModel w) = mkWidget w
          handler _ _ (AppModel w) (AppEvent (Chan ch) d) = 
            let inp = (InputValue ch d) in unsafePerformIO $ do
               progressPromoteStoreAtomic inp
               return ([Monomer.Model (AppModel (progressInternal inp w))])
          config = [
                Monomer.appWindowTitle "Test",
                Monomer.appTheme Monomer.lightTheme,
                Monomer.appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf"
                ]