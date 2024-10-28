
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}


module WidgetRattus.Widgets.InternalTypes where

import WidgetRattus
import WidgetRattus.InternalPrimitives
import WidgetRattus.Signal
import Data.Text

import qualified Monomer as M
{-# ANN module AllowLazyData #-}

-- The Displayable typeclass is used to define the display function.
-- The display function is used to convert a datatype to Text.
class Stable a => Displayable a where
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

-- The IsWidget typeclass is used to define the mkWidgetNode function.
class Continuous a => IsWidget a where
      mkWidgetNode :: a -> M.WidgetNode AppModel AppEvent
      
      mkWidget :: a -> Widget
      mkWidget w = Widget w (WidgetRattus.Signal.const True)


      setEnabled :: a -> Sig Bool -> Widget
      setEnabled = Widget

-- Custom data types for widgets.
data Widget where
    Widget :: IsWidget a => !a -> !(Sig Bool) -> Widget

data HStack where 
      HStack :: IsWidget a => !(Sig (List a)) -> HStack

data VStack where 
      VStack :: IsWidget a => !(Sig (List a)) -> VStack

data TextDropdown = TextDropdown {tddCurr :: !(Sig Text), tddEvent :: !(Chan Text), tddList :: !(Sig (List Text))}

data Popup = Popup {popCurr :: !(Sig Bool), popEvent :: !(Chan Bool), popChild :: !(Sig Widget)}

data Slider = Slider {sldCurr :: !(Sig Int), sldEvent :: !(Chan Int), sldMin :: !(Sig Int), sldMax :: !(Sig Int)}

data Button where
    Button :: (Displayable a) =>  {btnContent :: !(Sig a) , btnClick :: !(Chan ())} -> Button


data Label where
      Label :: (Displayable a) => {labText :: !(Sig a)} -> Label

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
      mkWidgetNode Button{btnContent = txt ::: _ , btnClick = click} =
            M.button  (display txt) (AppEvent click ())

instance IsWidget TextField where
      mkWidgetNode TextField{tfContent = txt ::: _, tfInput = inp} = 
            M.textFieldV txt (AppEvent inp)

instance IsWidget Label where
      mkWidgetNode Label{labText = txt ::: _} = M.label (display txt)


instance IsWidget HStack where
      mkWidgetNode (HStack ws) = M.hstack_ [ M.childSpacing_ 2] (reverse' $ fmap mkWidgetNode (current ws))

instance IsWidget VStack where
      mkWidgetNode (VStack ws) = M.vstack_ [ M.childSpacing_ 2] (reverse' $ fmap mkWidgetNode (current ws))

instance IsWidget TextDropdown where
      mkWidgetNode TextDropdown{tddList = opts ::: _, tddCurr = curr ::: _, tddEvent = ch}
            = M.textDropdownV curr (AppEvent ch) opts

instance IsWidget Popup where
      mkWidgetNode Popup{popCurr = curr ::: _, popEvent = ch, popChild = child}
            = M.popupV curr (AppEvent ch) (mkWidgetNode (current child))

instance IsWidget Slider where
      mkWidgetNode Slider{sldCurr = curr ::: _, sldEvent = ch, sldMin = min ::: _, sldMax = max ::: _}
            = M.hsliderV curr (AppEvent ch) min max

instance IsWidget Widget where
    mkWidgetNode (Widget w (e ::: _)) = M.nodeEnabled (mkWidgetNode w) e

    mkWidget w = w

    setEnabled (Widget w _) es = Widget w es