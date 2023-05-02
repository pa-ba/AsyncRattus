{-# LANGUAGE TypeOperators #-}

module Main (module Main) where

import Rattus
import Rattus.Stream as Str

{-
{-# ANN module Rattus #-}

twice :: Str Int -> Str Int
twice = Str.map (box (+1)) . Str.map (box (+1))

scanAndMap :: Str Int -> Str Int
scanAndMap xs = Str.map (box (+1)) (scan (box (+)) 0 xs)

sums :: Str Int -> Str Int
sums xs = scan (box (+)) 0 xs

twiceScan :: Str Int -> Str Int
twiceScan xs = scan (box (+)) 0  (scan (box (+)) 0 xs)

twiceScanMap :: Str Int -> Str Int
twiceScanMap xs = scan (box (+)) 0  (scanMap (box (+)) (box (+1)) 0 xs)

zipMap :: Str Int -> Str Int -> Str Int
zipMap xs ys = Str.map (box (\ (x:*y) -> x + y)) (Str.zip xs ys)

constMap :: Str Int
constMap = Str.map (box (+1)) (Str.const 5)



apply :: O (Int -> Int -> Int) -> O Int -> O Int -> O Int
apply f x y = f <#> x <#> y


apply' :: O (Int -> Int -> Int) -> Int -> O Int -> O Int
apply' f x y = f <## x <#> y

-}
{-# ANN main NotRattus #-}
main = putStrLn "This is just to test the rewrite rules"
