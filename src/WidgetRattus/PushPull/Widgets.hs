{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module WidgetRattus.PushPull.Widgets where

import WidgetRattus.Behaviour
import WidgetRattus.Event
import WidgetRattus
import qualified WidgetRattus.Widgets.InternalTypes as WR
import qualified WidgetRattus.Widgets as WR
import WidgetRattus.Widgets (Displayable)
import Data.Text hiding (zipWith)
import WidgetRattus.Signal (Sig ((:::)), map)
import Prelude hiding (max,const,zipWith)
import qualified Prelude 



class (Continuous a) => IsWidget a where
  mkOldWidget :: a -> C WR.Widget

  setEnabled :: a -> Beh Bool -> Widget
  setEnabled = Widget


data Widget where
  Widget :: IsWidget a => !a -> !(Beh Bool) -> Widget


continuous ''Widget
instance IsWidget Widget where
  mkOldWidget (Widget w beh) = do
    beh' <- discretize beh
    w' <- mkOldWidget w
    return (WR.Widget w' beh')

class Widgets ws where
      toWidgetList :: ws -> C (List WR.Widget)

instance {-# OVERLAPPABLE #-} IsWidget w => Widgets w where
      toWidgetList w = do
        w' <- mkOldWidget w
        return [ w' ]

instance {-# OVERLAPPING #-} (Widgets w, Widgets v) => Widgets (w :* v) where
      toWidgetList (w :* v) = do
        left <- toWidgetList w
        right <- toWidgetList v
        return $ left +++ right


instance {-# OVERLAPPING #-} (Widgets w) => Widgets (List w) where
      toWidgetList w = do
        toWidgetList w

instance {-# OVERLAPPABLE #-} (WR.IsWidget a, Continuous a) => IsWidget a where
  mkOldWidget a = do
    let w = WR.mkWidget a
    return w

-- HStack 
data HStack where
      HStack :: WR.IsWidget a => !(Beh (List a)) -> HStack


continuous ''HStack

instance IsWidget HStack where
      mkOldWidget (HStack ws) = do
        ws' <- discretize ws
        return $ WR.mkWidget (WR.HStack ws')

mkHStack :: WR.IsWidget a => Beh(List a) -> C HStack
mkHStack wl = do
      return (HStack wl)

mkConstHStack :: Widgets ws => ws -> C HStack
mkConstHStack w = do
  ws <- toWidgetList w
  mkHStack $ const ws

-- VStack
data VStack where
  VStack :: WR.IsWidget a => !(Beh (List a)) -> VStack

continuous ''VStack
instance IsWidget VStack where
  mkOldWidget (VStack ws) = do
    ws' <- discretize ws
    return $ WR.mkWidget (WR.VStack ws')

mkVStack :: WR.IsWidget a => Beh(List a) -> C VStack
mkVStack wl = do
      return (VStack wl)

mkConstVStack :: Widgets ws => ws -> C VStack
mkConstVStack w = do
  ws <- toWidgetList w
  mkVStack $ const ws


-- TextDropDown
data TextDropdown =
  TextDropdown {tddCurr :: !(Beh Text), tddEvent :: !(Chan Text), tddList :: !(Beh (List Text))}

continuous ''TextDropdown
instance IsWidget TextDropdown where
  mkOldWidget (TextDropdown cur ev list) = do
    cur' <- discretize cur
    list' <- discretize list
    return $ WR.mkWidget (WR.TextDropdown cur' ev list')

mkTextDropdown :: Beh (List Text) -> Text -> C TextDropdown
mkTextDropdown opts initial = do
  c <- chan
  let beh = stepper initial $ mkEv (box (wait c))
  return $ TextDropdown beh c opts


-- Popup
data Popup =
  Popup {popCurr :: !(Beh Bool), popEvent :: !(Chan Bool), popChild :: !(Beh WR.Widget)}

continuous ''Popup
instance IsWidget Popup where
      mkOldWidget (Popup curr ch child) = do
        curr' <- discretize curr
        child' <- discretize child
        return $ WR.mkWidget (WR.Popup curr' ch child')

mkPopup :: Ev Bool -> Beh WR.Widget -> C Popup
mkPopup initialVisibility w = do
      c <- chan
      let changeEvent = mkEv (box (wait c))
      let visibility = stepper False $ interleave (box Prelude.const) initialVisibility changeEvent
      return Popup{popCurr = visibility, popEvent = c, popChild = w}


-- Slider
data Slider =
  Slider {sldCurr :: !(Beh Int), sldEvent :: !(Chan Int), sldMin :: !(Beh Int), sldMax :: !(Beh Int)}

continuous ''Slider
instance IsWidget Slider where
  mkOldWidget (Slider curr ev min max) = do
    curr' <- discretize curr
    min' <- discretize min
    max' <- discretize max
    return $ WR.mkWidget (WR.Slider curr' ev min' max')

mkSlider :: Int -> Beh Int -> Beh Int -> C Slider
mkSlider start min max = do
  c <- chan
  let curr = stepper start $ mkEv (box (wait c))
  return $ Slider curr c min max


-- Button
data Button where
  Button :: (Displayable a) => {btnClick :: !(Chan ()), btnContent :: !(Beh a)} -> Button

continuous ''Button
instance IsWidget Button where
  mkOldWidget (Button click b) = do
    w <- discretize b
    return $ WR.mkWidget (WR.Button w click)

mkButton :: (Displayable a) => Beh a -> C Button
mkButton t = do
   c <- chan
   return $ Button c t


-- Label
data Label where
      Label :: (Displayable a) => {labText :: !(Beh a)} -> Label

continuous ''Label
instance IsWidget Label where
  mkOldWidget (Label t) = do
    t' <- discretize t
    return $ WR.mkWidget (WR.Label t')

mkLabel :: (Displayable a) => Beh a -> C Label
mkLabel t = do
  return $ Label t


-- TextField
data TextField = TextField {tfContent :: !(Beh Text), tfInput :: !(Chan Text)}

continuous ''TextField
instance IsWidget TextField where
  mkOldWidget (TextField b inp) = do
    txt <- discretize b
    return $ WR.mkWidget (WR.TextField txt inp)

mkTextField :: Text -> C TextField
mkTextField txt = do
  c <- chan
  let (Dense d) = mkEv (box (wait c))
  let beh = Beh $ WidgetRattus.Signal.map (box K) (txt ::: d)
  return $ TextField beh c

-- ProgressBar
mkProgressBar :: Beh Int -> Beh Int -> Beh Int -> C Slider
mkProgressBar min max curr = do
      c <- chan
      let boundedCurrent = zipWith (box Prelude.min) curr max
      return Slider{sldCurr = boundedCurrent, sldEvent = c, sldMin = min, sldMax = max}

btnOnClick :: Button -> Box(O())
btnOnClick b =
      let ch = btnClick b
      in box (wait ch)

btnOnClickEv :: Button -> Ev ()
btnOnClickEv b = mkEv (btnOnClick b)


tfInputEv :: TextField -> Ev Text
tfInputEv tf =
  let ch = tfInput tf
  in mkEv (box (wait ch))

setInputBehTF :: TextField -> Beh Text -> TextField
setInputBehTF tf b =
  tf{tfContent = b}

sliderOnChange :: Slider -> Ev Int
sliderOnChange s =
  let ch = sldEvent s
  in mkEv (box (wait ch))

mkConstText :: String -> Beh Text
mkConstText s = const (pack s)

runApplication :: IsWidget a => C a -> IO()
runApplication w =
  WR.runApplication ( do
        w' <- w
        mkOldWidget w'
    )
