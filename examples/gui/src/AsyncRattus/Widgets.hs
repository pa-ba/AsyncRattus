{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use void" #-}

module AsyncRattus.Widgets where

import AsyncRattus
import AsyncRattus.Signal
import AsyncRattus.Channels ( chan, wait, C(C), Chan )
import Data.Text
import Data.Char (isDigit)
import AsyncRattus.InternalPrimitives
import System.IO.Unsafe
import Control.Concurrent hiding (Chan)
import Data.IntSet as IntSet

import qualified Monomer

-- The Displayable typeclass is used to define the display function.
-- The display function is used to convert a datatype to Text.
class Displayable a where
      display :: a -> Text

-- The identity function.
instance Displayable Text where
      display :: Text -> Text
      display x = x

-- Convert Int to Text via String.
instance Displayable Int where  
      display x = pack (show x)

-- The IsWidget typeclass is used to define the mkWidget function.
class Continuous a => IsWidget a where
      mkWidget :: a -> Monomer.WidgetNode AppModel AppEvent

-- The AppModel datatype used to contain the Widget passed to runApplication. 
-- The associated clock is a set of timers. 
-- Any timers created with mkTimerEvent will be added to the clock.
data AppModel where
    AppModel :: IsWidget a => !a -> !Clock -> AppModel

-- Instance decleration for AppModel making it possible to
-- compare two AppModels.
instance (Eq AppModel) where
      (==) :: AppModel -> AppModel -> Bool
      _ == _ = False

-- AppEvent data type used to convert channels into events.
data AppEvent where
      AppEvent :: !(Chan a) -> !a -> AppEvent

-- Coustom data types for widgets.
data Widget where
    Widget :: IsWidget a => !a -> !(Sig Bool) -> Widget

data Button where
    Button :: (Displayable a, Stable a) =>  {btnContent :: !(Sig a) , btnClick :: !(Chan ())} -> Button

data TextField = TextField {tfContent :: !(Sig Text), tfInput :: !(Chan Text)} 

data Label where
      Label :: (Displayable a, Stable a) => {labText :: !(Sig a)} -> Label

data HStack = HStack {hGrp :: !(Sig (List Widget))}

data VStack = VStack {vGrp :: !(Sig (List Widget))}

data TextDropdown = TextDropdown {tddCurr :: !(Sig Text), tddEvent :: !(Chan Text), tddList :: !(Sig (List Text))}

data Popup = Popup {popCurr :: !(Sig Bool), popEvent :: !(Chan Bool), popChild :: !(Sig Widget)}

data Slider = Slider {sldCurr :: !(Sig Int), sldEvent :: !(Chan Int), sldMin :: !(Sig Int), sldMax :: !(Sig Int)}

-- Template Haskell code for generating instances of Continous.
continuous ''Button
continuous ''TextField
continuous ''Label
continuous ''Widget
continuous ''HStack
continuous ''VStack
continuous ''TextDropdown
continuous ''Popup
continuous ''Slider

-- isWidget Instance declerations for Widgets.
-- Here widgget data types are passed to Monomer constructors.
instance IsWidget Button where
      mkWidget :: Button -> Monomer.WidgetNode AppModel AppEvent
      mkWidget Button{btnContent = txt ::: _ , btnClick = click} =
            Monomer.button  (display txt) (AppEvent click ())

instance IsWidget TextField where
      mkWidget :: TextField -> Monomer.WidgetNode AppModel AppEvent
      mkWidget TextField{tfContent = txt ::: _, tfInput = inp} = 
            Monomer.textFieldV txt (AppEvent inp)

instance IsWidget Label where
      mkWidget :: Label -> Monomer.WidgetNode AppModel AppEvent
      mkWidget Label{labText = txt ::: _} = Monomer.label (display txt)


instance IsWidget HStack where
      mkWidget :: HStack -> Monomer.WidgetNode AppModel AppEvent
      mkWidget HStack{hGrp = ws} = Monomer.hstack (fmap mkWidget (current ws))

instance IsWidget VStack where
      mkWidget :: VStack -> Monomer.WidgetNode AppModel AppEvent
      mkWidget VStack{vGrp = ws} = Monomer.vstack (fmap mkWidget (current ws))

instance IsWidget TextDropdown where
      mkWidget :: TextDropdown -> Monomer.WidgetNode AppModel AppEvent
      mkWidget TextDropdown{tddList = opts ::: _, tddCurr = curr ::: _, tddEvent = ch}
            = Monomer.textDropdownV curr (AppEvent ch) opts

instance IsWidget Popup where
      mkWidget :: Popup -> Monomer.WidgetNode AppModel AppEvent
      mkWidget Popup{popCurr = curr ::: _, popEvent = ch, popChild = child}
            = Monomer.popupV curr (AppEvent ch) (mkWidget (current child))

instance IsWidget Slider where
      mkWidget :: Slider -> Monomer.WidgetNode AppModel AppEvent
      mkWidget Slider{sldCurr = curr ::: _, sldEvent = ch, sldMin = min ::: _, sldMax = max ::: _}
            = Monomer.hsliderV curr (AppEvent ch) min max

instance IsWidget Widget where
    mkWidget :: Widget -> Monomer.WidgetNode AppModel AppEvent
    mkWidget (Widget w (e ::: _)) = Monomer.nodeEnabled (mkWidget w) e


-- Function to construct a Widget that never gets disabled
enabledWidget :: IsWidget a => a -> Widget
enabledWidget w = Widget w (AsyncRattus.Signal.const True)

-- Functions for constructing Async Rattus widgets. 
mkButton :: (Displayable a, Stable a) => Sig a -> C Button
mkButton t = do
      c <- chan
      return Button{btnContent = t, btnClick = c}

mkTextField :: Text -> C TextField
mkTextField txt = do
      c <- chan
      let sig = txt ::: mkSig (box (wait c))
      return TextField{tfContent = sig, tfInput = c}

mkLabel :: (Displayable a, Stable a) => Sig a -> C Label
mkLabel t = do
      return Label{labText = t}

mkHStack :: Sig(List Widget) -> C HStack
mkHStack wl = do
      return HStack{hGrp = wl}

mkVStack :: Sig(List Widget) -> C VStack
mkVStack wl = do
      return VStack{vGrp = wl}

mkTextDropdown :: Sig (List Text) -> Text -> C TextDropdown
mkTextDropdown opts init = do
      c <- chan
      let curr = init ::: mkSig (box (wait c))
      return TextDropdown{tddCurr = curr, tddEvent = c, tddList = opts}

mkPopup :: Sig Bool -> Sig Widget -> C Popup 
mkPopup b w = do
      c <- chan
      let sig = current b ::: interleave (box (\x y -> x)) (future b) (mkSig (box (wait c)))
      return Popup{popCurr = sig, popEvent = c, popChild = w}

mkSlider :: Int -> Sig Int -> Sig Int -> C Slider
mkSlider start min max = do
      c <- chan
      let curr = start ::: mkSig (box (wait c))
      return Slider{sldCurr = curr, sldEvent = c, sldMin = min, sldMax = max}

mkProgressBar :: Sig Int -> Sig Int -> Sig Int -> C Slider
mkProgressBar min max curr = do
      c <- chan
      let boundedCurrent = AsyncRattus.Signal.zipWith (box Prelude.min) curr max
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
      let leaved = current (tfContent tf) ::: interleave (box (\x y -> x)) (future (tfContent tf)) sig
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
runApplication :: IsWidget a => C a -> IO ()
runApplication (C w) = do
    w' <- w
    let cl = nextProgress w' 
    Monomer.startApp (AppModel w' emptyClock) handler builder config
    where builder _ (AppModel w _) = mkWidget w
          handler _ _ (AppModel w cl) (AppEvent (Chan ch) d) =
            let inp = OneInput ch d in unsafePerformIO $ do
               progressPromoteStoreAtomic inp
               let (w' , cl') = progressAndNext inp w
               let activeTimers = if ch > 0 then IntSet.delete ch cl else cl
               let newTimers = IntSet.filter (> 0) cl' `IntSet.difference` activeTimers
               let timers = Prelude.map (Monomer.Producer . mkTimerEvent) (IntSet.toList newTimers)
               return (Monomer.Model (AppModel w' (newTimers `IntSet.union` activeTimers)) : Monomer.Request Monomer.RenderOnce : timers )
          config = [
                Monomer.appWindowTitle "GUI Application",
                Monomer.appTheme Monomer.lightTheme,
                Monomer.appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
                Monomer.appInitEvent (AppEvent (Chan 1) ())
                ]
