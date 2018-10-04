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
main = 
    do printboard testBoard False -- change me :) 





---------------------------- BOARD FORMATTING STUFF ------------------------------

-- Displays a formatted board to the user. Only displays unhit ships if it is the user's own board. 
printboard :: [[Int]] -> Bool -> IO ()
printboard board ownBoard =
    do
    T.putStrLn $ tabl EnvAscii hdecor vdecor aligns formattedboard
    where
        hdecor = DecorAll
        vdecor = DecorAll
        aligns = []
        formattedboard = boardtotext board ownBoard

----------  HELPER METHODS ------------

-- Given a 2D array of integers representing a board, convert to a 2D array of text characters 
boardtotext :: [[Int]] -> Bool -> [[T.Text]]
firstrow = (" ":[T.singleton a | a <- ['A'..'J']]) 
boardtotext board ownBoard = firstrow :
                              [(intToText i):(rowToSymbol (board !! (i - 1)) ownBoard) | i <- [1..10]]

-- Convert a row of integers to a row of corresponding text characters
rowToSymbol :: [Int] -> Bool -> [T.Text]
rowToSymbol row ownBoard = map (\ i -> getboardsymbol i ownBoard) row

-- Convert an integer representing a square state to a symbol to show to the user 
-- Only show unhit ships if ownBoard is True
getboardsymbol :: Int -> Bool -> T.Text
getboardsymbol i ownBoard
    | i == 0    = " "
    | i == 1    = "✕"
    | i == 2    = if ownBoard then "☆" else "✕"   
    | i == 3    = "★"

-- Convert int to T.Text object 
intToText :: Int -> T.Text
intToText i = T.pack (show i)
