{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Tabl
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- Boards are 10x10 2D arrays with integers in the range [0,3], where:
-- 0 means the square is empty and has not been hit
-- 1 means a miss 
-- 2 means an unhit ship 
-- 3 means a hit ship 
playerboard = replicate 10 (replicate 10 0)
aiboard = replicate 10 (replicate 10 0)

main :: IO ()
main = do
    T.putStrLn $ tabl EnvAscii hdecor vdecor aligns board
    where
        hdecor = DecorAll
        vdecor = DecorAll
        aligns = []
        board = showboard [[1]]

firstrow = (" ":[T.singleton a | a <- ['A'..'J']]) 
showboard :: [[Int]] -> [[T.Text]]
showboard board = firstrow:[(T.pack (show i)):(replicate 10 " ") | i <- [1..10]]
