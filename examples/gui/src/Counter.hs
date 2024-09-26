{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS -fplugin=WidgetRattus.Plugin #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use const" #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Counter where
import WidgetRattus
import WidgetRattus.Signal ( Sig, const, map, mkSig, scanAwait )
import WidgetRattus.Channels
import WidgetRattus.Widgets

import Control.Concurrent ( forkIO )
import Control.Monad
import Prelude hiding (map, const, zipWith, zip, filter, getLine, putStrLn,null)
import Data.Text.IO
import Data.Text hiding (filter, map, all)
import qualified Data.Text as Text

--Benchmark 1
benchmark1 :: C VStack
benchmark1 = do 
    btn <- mkButton (const ("Increment" :: Text))
    let sig = btnOnClickSig btn
    let sig' = scanAwait (box (\ n _ -> n+1 :: Int)) 0 sig 
    lbl <- mkLabel sig'
    mkVStack (const [enabledWidget lbl, enabledWidget btn])
    
