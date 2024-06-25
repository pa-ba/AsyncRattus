{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS -fplugin=AsyncRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use const" #-}

module FlightBooker where
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
import Data.Text (Text, unpack, splitOn)
import Text.Read (readMaybe)
import Data.Maybe (isNothing, isJust)

-- Benchmark 3
isDate :: Text -> Bool
isDate txt = case splitOn "-" txt of
  [dayStr, monthStr, yearStr] ->
    let day = readMaybe (unpack dayStr) :: Maybe Int
        month = readMaybe (unpack monthStr) :: Maybe Int
        year = readMaybe (unpack yearStr) :: Maybe Int
    in isValid day month year
  _ -> False
  where
    isValid :: Maybe Int -> Maybe Int -> Maybe Int -> Bool
    isValid (Just d) (Just m) (Just y)
      | m < 1 || m > 12 = False
      | d < 1 || d > daysInMonth m y = False
      | otherwise = True
    isValid _ _ _ = False

    daysInMonth :: Int -> Int -> Int
    daysInMonth m y
        | m `Prelude.elem` ([4, 6, 9, 11] :: [Int]) = 30
        | m == 2 = if isLeapYear y then 29 else 28
        | otherwise = 31

    isLeapYear :: Int -> Bool
    isLeapYear y = y `mod` 4 == 0 && (y `mod` 100 /= 0 || y `mod` 400 == 0)

isLater :: Text -> Text -> Bool
isLater dep ret = case (splitOn "-" dep, splitOn "-" ret) of
  ([depDayStr, depMonthStr, depYearStr], [retDayStr, retMonthStr, retYearStr]) ->
    let depDay = readMaybe (unpack depDayStr) :: Maybe Int
        depMonth = readMaybe (unpack depMonthStr) :: Maybe Int
        depYear = readMaybe (unpack depYearStr) :: Maybe Int
        retDay = readMaybe (unpack retDayStr) :: Maybe Int
        retMonth = readMaybe (unpack retMonthStr) :: Maybe Int
        retYear = readMaybe (unpack retYearStr) :: Maybe Int
    in all isJust ([depDay, depMonth, depYear, retDay, retMonth, retYear] :: [Maybe Int]) &&
       (depYear < retYear ||
       (depYear == retYear && (depMonth < retMonth ||
       (depMonth == retMonth && depDay < retDay))))
  _ -> False

bookingToText :: Bool -> Text -> Text -> Text
bookingToText oneWay dep ret =
  "You have booked a " <> if oneWay then "one-way flight on " <> dep
  else "return flight from " <> dep <> " to " <> ret

benchmark3 :: C VStack
benchmark3 = do
    dropDown <- mkTextDropdown (const ["One-Way", "Return-Flight"]) "One-Way"
    tf1 <- mkTextField "01-01-2021"
    tf2 <- mkTextField "01-02-2021"
    button <- mkButton (const  ("Book" :: Text))

    let isRF = map (box (== "Return-Flight")) (tddCurr dropDown)
    let isOW = map (box (== "One-Way")) (tddCurr dropDown)
    
    let labelSig = AsyncRattus.Signal.zipWith3 (box bookingToText) isOW (tfContent tf1) (tfContent tf2)

    let sig = scanAwait (box (\ b _ -> True )) False (btnOnClickSig button)

    label <- mkLabel labelSig
    
    popup <- mkPopup sig (const (enabledWidget label))

    let tf1IsDate = map (box isDate) (tfContent tf1)
    let tf2IsDate = map (box isDate) (tfContent tf2)
    let tf1IsLater = AsyncRattus.Signal.zipWith (box isLater) (tfContent tf1) (tfContent tf2)

    let oneWayAndDate = AsyncRattus.Signal.zipWith (box (&&)) isOW tf1IsDate
    let returnFlightAndIsLater = AsyncRattus.Signal.zipWith (box (&&)) isRF tf1IsLater
    let validBooking = AsyncRattus.Signal.zipWith (box (||)) oneWayAndDate returnFlightAndIsLater

    mkVStack (const
        [enabledWidget popup, enabledWidget dropDown,
         enabledWidget tf1, Widget tf2 isRF, Widget button validBooking])