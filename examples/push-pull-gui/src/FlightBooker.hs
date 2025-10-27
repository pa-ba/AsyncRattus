{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import WidgetRattus.PushPull.Widgets
import WidgetRattus
import WidgetRattus.Behaviour
import WidgetRattus.Event
import Data.Text (Text)
import Prelude hiding (zipWith3, const, map,zipWith)

import WidgetRattus.Widgets ()

isDate :: Text -> Bool
isDate txt = case splitOn' "-" txt of
  [dayStr, monthStr, yearStr] ->
    let day = readMaybe' dayStr
        month = readMaybe' monthStr
        year = readMaybe' yearStr
    in isValid day month year
  _ -> False
  where
    isValid :: Maybe' Int -> Maybe' Int -> Maybe' Int -> Bool
    isValid (Just' d) (Just' m) (Just' y)
      | m < 1 || m > 12 = False
      | d < 1 || d > daysInMonth m y = False
      | otherwise = True
    isValid _ _ _ = False

    daysInMonth :: Int -> Int -> Int
    daysInMonth m y
        | m `elem` ([4, 6, 9, 11] :: List Int) = 30
        | m == 2 = if isLeapYear y then 29 else 28
        | otherwise = 31

    isLeapYear :: Int -> Bool
    isLeapYear y = y `mod` 4 == 0 && (y `mod` 100 /= 0 || y `mod` 400 == 0)

isLater :: Text -> Text -> Bool
isLater dep ret = case (splitOn' "-" dep, splitOn' "-" ret) of
  ([depDayStr, depMonthStr, depYearStr], [retDayStr, retMonthStr, retYearStr]) ->
    let depDay = readMaybe' depDayStr
        depMonth = readMaybe' depMonthStr
        depYear = readMaybe' depYearStr
        retDay = readMaybe' retDayStr
        retMonth = readMaybe' retMonthStr
        retYear = readMaybe' retYearStr
    in all isJust' ([depDay, depMonth, depYear, retDay, retMonth, retYear] :: List (Maybe' Int)) &&
       (depYear < retYear ||
       (depYear == retYear && (depMonth < retMonth ||
       (depMonth == retMonth && depDay < retDay))))
  _ -> False
  
bookingToText :: Bool -> Text -> Text -> Text
bookingToText oneWay dep ret =
  "You have booked a " <> if oneWay then "one-way flight on " <> dep
  else "return flight from " <> dep <> " to " <> ret

flightBooker :: C VStack
flightBooker = do
      -- Input UI
      flightTypeDropdown <- mkTextDropdown (const ["One-Way", "Return-Flight"]) "One-Way"
      departureDateField <- mkTextField "01-01-2021"
      returnDateField <- mkTextField "01-02-2021"
      bookButton <- mkButton (mkConstText "Book")
      
      -- Flight type checker
      let isReturnFlight = mapB (box (== "Return-Flight")) (tddCurr flightTypeDropdown)
      let isOneWayFlight = mapB (box (== "One-Way")) (tddCurr flightTypeDropdown)
      
      -- Popup
      let bookingSummary = zipWith3 (box bookingToText) isOneWayFlight (tfContent departureDateField) (tfContent returnDateField)

      let samplePopup = scan (box (\_ _ -> True)) False (btnOnClickEv bookButton)
      
      summaryLabel <- mkLabel bookingSummary
      summaryLabel' <- (mkDiscrWidget (mkWidget summaryLabel))
      summaryPopup <- mkPopup samplePopup (const summaryLabel')

      -- Valid booking checker
      let departureDateFieldIsDate = mapB (box isDate) (tfContent departureDateField)
      let departureDateFieldIsLater = zipWith (box isLater) (tfContent departureDateField) (tfContent returnDateField)

      let oneWayAndDate = zipWith (box (&&)) isOneWayFlight departureDateFieldIsDate
      let returnFlightAndIsLater = zipWith (box (&&)) isReturnFlight departureDateFieldIsLater
      let validBooking = zipWith (box (||)) oneWayAndDate returnFlightAndIsLater

      -- UI
      mkConstVStack (summaryPopup :* flightTypeDropdown :* departureDateField :* setEnabled returnDateField isReturnFlight :* setEnabled bookButton validBooking)

main :: IO ()
main = runApplication flightBooker