{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeOperators #-}


module WidgetRattus.Widgets 
  ( Displayable (..)
  , IsWidget (..)
  , Widgets (..)
  , Widget
  , HStack
  , VStack
  , TextDropdown
  , tddCurr
  , tddEvent
  , tddList
  , Popup
  , popCurr
  , popEvent
  , popChild
  , Slider
  , sldCurr
  , sldEvent
  , sldMin
  , sldMax
  , Button
  , btnContent
  , btnClick
  , Label
  , labText
  , TextField
  , tfContent
  , tfInput
  , mkButton
  , mkTextField
  , addInputSigTF
  , mkLabel
  , mkHStack
  , mkConstHStack
  , mkVStack
  , mkConstVStack
  , mkTextDropdown
  , mkPopup
  , mkSlider
  , mkProgressBar
  , btnOnClick
  , btnOnClickSig
  , setInputSigTF
  , textFieldOnInput
  , textFieldOnInputSig
  , runApplication
      ) where

import WidgetRattus
import WidgetRattus.Widgets.InternalTypes
import WidgetRattus.Signal
import Data.Text
import WidgetRattus.InternalPrimitives
import System.IO.Unsafe
import Control.Concurrent hiding (Chan)
import Data.IntSet as IntSet
import Prelude hiding (const)

import qualified Monomer as M

-- The identity function.
instance Displayable Text where
      display x = x

-- Convert Int to Text via String.
instance Displayable Int where  
      display x = toText x


-- Functions for constructing Async Rattus widgets. 
mkButton :: (Displayable a) => Sig a -> C Button
mkButton t = do
      c <- chan
      return Button{btnContent = t, btnClick = c}

mkTextField :: Text -> C TextField
mkTextField txt = do
      c <- chan
      let sig = txt ::: mkSig (box (wait c))
      return TextField{tfContent = sig, tfInput = c}

mkLabel :: (Displayable a) => Sig a -> C Label
mkLabel t = do
      return Label{labText = t}

class Widgets ws where
      toWidgetList :: ws -> List Widget

instance {-# OVERLAPPABLE #-} IsWidget w => Widgets w where
      toWidgetList w = [ mkWidget w ]

instance {-# OVERLAPPING #-} (Widgets w, Widgets v) => Widgets (w :* v) where
      toWidgetList (w :* v) = toWidgetList w +++ toWidgetList v


instance {-# OVERLAPPING #-} (Widgets w) => Widgets (List w) where
      toWidgetList w = concatMap' toWidgetList w


mkHStack :: IsWidget a => Sig(List a) -> C HStack
mkHStack wl = do
      return (HStack wl)
      
mkConstHStack :: Widgets ws => ws -> C HStack
mkConstHStack w = mkHStack (const (toWidgetList w))

mkVStack :: IsWidget a => Sig(List a) -> C VStack
mkVStack wl = do
      return (VStack wl)

mkConstVStack :: Widgets ws => ws -> C VStack
mkConstVStack w = mkVStack (const (toWidgetList w))

mkTextDropdown :: Sig (List Text) -> Text -> C TextDropdown
mkTextDropdown opts init = do
      c <- chan
      let curr = init ::: mkSig (box (wait c))
      return TextDropdown{tddCurr = curr, tddEvent = c, tddList = opts}

mkPopup :: Sig Bool -> Sig Widget -> C Popup 
mkPopup b w = do
      c <- chan
      let sig = current b ::: interleave (box (\x _ -> x)) (future b) (mkSig (box (wait c)))
      return Popup{popCurr = sig, popEvent = c, popChild = w}

mkSlider :: Int -> Sig Int -> Sig Int -> C Slider
mkSlider start min max = do
      c <- chan
      let curr = start ::: mkSig (box (wait c))
      return Slider{sldCurr = curr, sldEvent = c, sldMin = min, sldMax = max}

mkProgressBar :: Sig Int -> Sig Int -> Sig Int -> C Slider
mkProgressBar min max curr = do
      c <- chan
      let boundedCurrent = WidgetRattus.Signal.zipWith (box Prelude.min) curr max
      return Slider{sldCurr = boundedCurrent, sldEvent = c, sldMin = min, sldMax = max}


-- Helper function that takes a Button and returns a boxed delayed computation.
-- The delayed computation is defined from the buttons input channel.
btnOnClick :: Button -> Box(O())
btnOnClick btn =
      let ch = btnClick btn
      in box (wait ch)

-- Function that constructs a delayed signal from a Button.
btnOnClickSig :: Button -> O (Sig ())
btnOnClickSig btn = mkSig (btnOnClick btn)

-- Creates a new textfield whose contents are determined by
-- the input signal.
-- Therefore user input will only be shown if the input signal
-- ticks in response to user input on the textfield.
-- Note: the input TF and output TF share an input channel
-- Hence if both are part of a GUI they will be written to simultaneously
setInputSigTF :: TextField -> Sig Text -> TextField
setInputSigTF tf sig = tf{tfContent = sig} 

-- Uses the input signal to create a new textfield
-- The returned textfield updates in response to the input signal
-- as well as the content signal of the original textfield.
-- Note: the input TF and output TF share an input channel
-- Hence if both are part of a GUI they will be written to simultaneously
addInputSigTF :: TextField -> O (Sig Text) -> TextField
addInputSigTF tf sig =
      let leaved = current (tfContent tf) ::: interleave (box (\x _ -> x)) (future (tfContent tf)) sig
      in tf{tfContent = leaved, tfInput = tfInput tf}

-- Helper function that takes a TextField and returns a boxed delayed computation.
-- The delayed computation is defined from the Textfields input channel.
textFieldOnInput :: TextField -> Box(O Text)
textFieldOnInput tf =
      let ch = tfInput tf
      in box (wait ch)

-- Function that constructs a delayed signal from a Textfield.
textFieldOnInputSig :: TextField -> O (Sig Text)
textFieldOnInputSig tf = mkSig (textFieldOnInput tf)


-- Function which creates a timed event. Associated clock will be part of the AppModel.
mkTimerEvent :: Int -> (AppEvent -> IO ()) -> IO ()
mkTimerEvent n cb = (threadDelay n >> cb (AppEvent (Chan n) ())) >> return ()


-- runApplication takes as input a widget and starts the GUI applicaiton
-- by calling Monomer's startApp function.
{-# ANN runApplication AllowLazyData #-}
runApplication :: IsWidget a => C a -> IO ()
runApplication (C w) = do
    w' <- w
    M.startApp (AppModel w' emptyClock) handler builder config
    where builder _ (AppModel w _) = mkWidgetNode w `M.styleBasic` [M.padding 3]
          handler _ _ (AppModel w cl) (AppEvent (Chan ch) d) =
            let inp = OneInput ch d in unsafePerformIO $ do
               progressPromoteStoreAtomic inp
               let (w' , cl') = progressAndNext inp w
               let activeTimers = if ch > 0 then IntSet.delete ch cl else cl
               let newTimers = IntSet.filter (> 0) cl' `IntSet.difference` activeTimers
               let timers = Prelude.map (M.Producer . mkTimerEvent) (IntSet.toList newTimers)
               return (M.Model (AppModel w' (newTimers `IntSet.union` activeTimers)) : M.Request M.RenderOnce : timers )
          config = [
                M.appWindowTitle "GUI Application",
                M.appTheme M.lightTheme,
                M.appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
                M.appInitEvent (AppEvent (Chan 1) ())
                ]
