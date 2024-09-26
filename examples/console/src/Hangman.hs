{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}

module Main (module Main) where
import WidgetRattus
import WidgetRattus.Signal
import WidgetRattus.Channels
import Control.Concurrent ( forkIO )
import Control.Monad
import Prelude hiding (map, const, zipWith, zip, filter, getLine, putStrLn, null)
import Data.Text.IO
import Data.Text hiding (filter, map)
import qualified Data.Text as Text (filter, map)
import System.Exit
import qualified Data.Char

{-# ANN consoleInput AllowRecursion #-}
consoleInput :: IO (Box (O (Sig Text)))
consoleInput = do
    (inp :* cb) <- getInputSig
    let loop = do 
            line <- getLine
            cb line
            loop
    forkIO loop
    return inp


setPrint :: (Producer p a, Show a) => p -> IO ()
setPrint sig = setOutput sig print


setQuit :: (Producer p a) => p -> IO ()
setQuit sig = setOutput sig (\ _ -> exitSuccess)

textOverlap :: Text -> Text -> Text
textOverlap word guess =
    Text.map (\ x -> if x `Data.Text.elem` guess then x else '_') word

readWord :: Text -> Maybe' Text
readWord text = if Data.Text.all Data.Char.isAscii text && (text /= "quit")
                then Just' (Data.Text.toLower text) else Nothing'

isVictory :: Text -> Text -> Maybe' Text
isVictory word guess =
    if word == Data.Text.toLower guess
    then Just' (append "Indeed the word was " (append word ". You won!"))
    else Nothing'
main :: IO ()
main = do
    console :: O (Sig Text) <- unbox <$> consoleInput
    wordSig :: O (Sig Text) <- unbox <$> filterMapAwait (box readWord) console
    quitSig :: O (Sig Text) <- unbox <$> filterAwait (box (== "quit")) console
    victorySig :: O (Sig Text) <- unbox <$> filterMapAwait (box (isVictory "wordtoguess")) console
    let mappedSig = mapAwait (box (textOverlap "wordtoguess")) wordSig
    let combSig = interleave (box (\x y -> y)) mappedSig victorySig
    setPrint combSig
    putStrLn "Welcome to Hangman!"
    putStrLn "A word has been hidden for you"
    putStrLn "Type quit to exit,"
    putStrLn "or any word to take a guess"
    setQuit quitSig
    startEventLoop
