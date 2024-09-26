
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module WidgetRattus.Widgets.Types where

import WidgetRattus
import WidgetRattus.InternalPrimitives
import WidgetRattus.Signal
import WidgetRattus.Channels ( chan, wait, C(C), Chan )
import Data.Text

import qualified Monomer
{-# ANN module AllowLazyData #-}

-- The Displayable typeclass is used to define the display function.
-- The display function is used to convert a datatype to Text.
class Displayable a where
      display :: a -> Text

-- The AppModel datatype used to contain the Widget passed to runApplication. 
-- The associated clock is a set of timers. 
-- Any timers created with mkTimerEvent will be added to the clock.
data AppModel where
    AppModel :: IsWidget a => !a -> !Clock -> AppModel



instance (Eq AppModel) where
      _ == _ = False


-- AppEvent data type used to convert channels into events.
data AppEvent where
      AppEvent :: !(Chan a) -> !a -> AppEvent

-- The IsWidget typeclass is used to define the mkWidget function.
class Continuous a => IsWidget a where
      mkWidget :: a -> Monomer.WidgetNode AppModel AppEvent
-- Coustom data types for widgets.
data Widget where
    Widget :: IsWidget a => !a -> !(Sig Bool) -> Widget

data HStack = HStack {hGrp :: !(Sig (List Widget))}

data VStack = VStack {vGrp :: !(Sig (List Widget))}

data TextDropdown = TextDropdown {tddCurr :: !(Sig Text), tddEvent :: !(Chan Text), tddList :: !(Sig (List Text))}

data Popup = Popup {popCurr :: !(Sig Bool), popEvent :: !(Chan Bool), popChild :: !(Sig Widget)}

data Slider = Slider {sldCurr :: !(Sig Int), sldEvent :: !(Chan Int), sldMin :: !(Sig Int), sldMax :: !(Sig Int)}

data Button where
    Button :: (Displayable a, Stable a) =>  {btnContent :: !(Sig a) , btnClick :: !(Chan ())} -> Button


data Label where
      Label :: (Displayable a, Stable a) => {labText :: !(Sig a)} -> Label

data TextField = TextField {tfContent :: !(Sig Text), tfInput :: !(Chan Text)} 

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
      mkWidget Button{btnContent = txt ::: _ , btnClick = click} =
            Monomer.button  (display txt) (AppEvent click ())

instance IsWidget TextField where
      mkWidget TextField{tfContent = txt ::: _, tfInput = inp} = 
            Monomer.textFieldV txt (AppEvent inp)

instance IsWidget Label where
      mkWidget Label{labText = txt ::: _} = Monomer.label (display txt)


instance IsWidget HStack where
      mkWidget HStack{hGrp = ws} = Monomer.hstack (fmap mkWidget (current ws))

instance IsWidget VStack where
      mkWidget VStack{vGrp = ws} = Monomer.vstack (fmap mkWidget (current ws))

instance IsWidget TextDropdown where
      mkWidget TextDropdown{tddList = opts ::: _, tddCurr = curr ::: _, tddEvent = ch}
            = Monomer.textDropdownV curr (AppEvent ch) opts

instance IsWidget Popup where
      mkWidget Popup{popCurr = curr ::: _, popEvent = ch, popChild = child}
            = Monomer.popupV curr (AppEvent ch) (mkWidget (current child))

instance IsWidget Slider where
      mkWidget Slider{sldCurr = curr ::: _, sldEvent = ch, sldMin = min ::: _, sldMax = max ::: _}
            = Monomer.hsliderV curr (AppEvent ch) min max

instance IsWidget Widget where
    mkWidget (Widget w (e ::: _)) = Monomer.nodeEnabled (mkWidget w) e