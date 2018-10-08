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

validLetters = ['A' .. 'J']
validNumbers = ['0' .. '9']

-- Takes in the letter character of a coordinate, and returns the integer mapping
-- NOTE: letter must be an element of validLetters.
convertLetterCoordinateToNum :: Char -> Int
convertLetterCoordinateToNum letter
    | letter == 'A' = 0
    | letter == 'B' = 1
    | letter == 'C' = 2
    | letter == 'D' = 3
    | letter == 'E' = 4
    | letter == 'F' = 5
    | letter == 'G' = 6
    | letter == 'H' = 7
    | letter == 'I' = 8
    | letter == 'J' = 9
    | otherwise = -1

main :: IO ()
main = 
    do
        putStrLn("Welcome to Battleship! Let's get started...")
        putStrLn("First we'll get your board setup.")
        putStrLn("When inputting coordinates, please use the form A1, J9, etc.")
        setupBoard <- setup
        printboard setupBoard True

---------------------------- BOARD SETUP ------------------------------

-- Guides the player through placing their ships on their board.
setup :: IO [[Int]]
setup = 
    do
        board1 <- placeShip 5 "Carrier" playerboard
        printboard board1 True
        board2 <- placeShip 4 "Battleship" board1
        printboard board2 True
        board3 <- placeShip 3 "Cruiser" board2
        printboard board3 True
        board4 <- placeShip 3 "Submarine" board3
        printboard board4 True
        placeShip 2 "Destroyer" board4

-- Guides the player through placing the ship identified by size and name on their board.
placeShip :: Int -> [Char] -> [[Int]] -> IO [[Int]]
placeShip size name board =
    do
        putStrLn("Let's place your " ++name++ ". It occupies " ++(show size)++ " spaces on the board.")
        putStrLn("What is the starting coordinate you would like?")
        start <- getLine
        if (isValidCoordinate start)
            then do
                let startCoordinate = createCoordinate start
                putStrLn("Give an end coordinate that is " ++(show (size - 1))++ " spaces away vertically or horizontally from " ++start++ ".")
                end <- getLine
                if (isValidCoordinate end) --first check if it's on the board
                    then do
                        let endCoordinate = createCoordinate end
                        if (isValidShipPlacement startCoordinate endCoordinate size board)
                            then do
                                let newBoard = updateBoardWithShip startCoordinate endCoordinate board size
                                return newBoard
                            else do
                                putStrLn("Invalid placement. Please try again, making sure your ships do not overlap and span the correct number of spaces.")
                                placeShip size name board
                    else do
                        putStrLn("That is an invalid coordinate. Please try again!")
                        placeShip size name board
            else do
                putStrLn("That is an invalid coordinate. Please try again.")
                placeShip size name board

-- Updates the board between the start and end coordinates to contain a ship.
-- Returns the new board with the ship placed.
updateBoardWithShip :: (Int, Int) -> (Int, Int) -> [[Int]] -> Int -> [[Int]]
updateBoardWithShip (sRow, sCol) (eRow, eCol) board size
    | sRow == eRow = updateRowWithShip board sRow (min sCol eCol) size
    | sCol == eCol = updateColWithShip board sCol (min sRow eRow) size

-- Updates the spaces of the board in the given row to contain a ship of
-- size num that starts at column start
updateRowWithShip :: [[Int]] -> Int -> Int -> Int -> [[Int]]
updateRowWithShip board row start num
    | num == 0 = board
    | otherwise = updateRowWithShip (updateBoardSquare board (row, start) 2) row (start + 1) (num - 1)

-- Updates the spaces of the board in the given column col to contain a ship of
-- size num that starts at row start
updateColWithShip :: [[Int]] -> Int -> Int -> Int -> [[Int]]
updateColWithShip board col start num
    | num == 0 = board
    | otherwise = updateColWithShip (updateBoardSquare board (start, col) 2) col (start + 1) (num - 1)

-- Update square (x,y) on the board to newval
updateBoardSquare :: [[Int]] -> (Int, Int) -> Int -> [[Int]]
updateBoardSquare board (row, col) newval = [[if i == row && j == col then newval else ((board !! i) !! j) | j <- [0..9]] | 
    i <- [0..9]]

-- Checks if the coordinate given is a valid board coordinate.
isValidCoordinate :: [Char] -> Bool
isValidCoordinate [letter, num] = ((toUpper letter) `elem` validLetters) && (num `elem` validNumbers)
isValidCoordinate lst = False

-- Checks if the given start and end coordinates are correct for a ship occupying
-- size spaces AND if the spaces between start and end are free.
isValidShipPlacement :: (Int, Int) -> (Int, Int) -> Int -> [[Int]] -> Bool
isValidShipPlacement (sRow, sCol) (eRow, eCol) size board
    | sCol == eCol = checkDifference sRow eRow (size - 1) && isColFreeBetween sRow eRow board sCol
    | sRow ==  eRow = checkDifference sCol eCol (size - 1) && isRowFreeBetween sCol eCol board sRow
    | otherwise = False

-- Checks if the spaces between (and including) columns start and end in the given row
-- are free (i.e. not occupied by a ship).
isRowFreeBetween :: Int -> Int -> [[Int]] -> Int -> Bool
isRowFreeBetween start end board row
    | start < end = and [isFreeSpace board (row, i) | i <- [start .. end]]
    | start > end = and [isFreeSpace board (row, i) | i <- [end .. start]]
    | otherwise = isFreeSpace board (row, start)

-- Checks if the spaces between (and including) rows start and end in the given column
-- are free (i.e. not occupied by a ship)
isColFreeBetween :: Int -> Int -> [[Int]] -> Int -> Bool
isColFreeBetween start end board col
    | start < end = and [isFreeSpace board (i, col) | i <- [start .. end]]
    | start > end = and [isFreeSpace board (i, col) | i <- [end .. start]]
    | otherwise = isFreeSpace board (start, col)

-- Returns true if the space at the given coordinate is empty/free, else false.
isFreeSpace :: [[Int]] -> (Int, Int) -> Bool
isFreeSpace board (row, col) = (getValueOfCoordinate board (row,col)) == 0

-- Returns the value on the board at the given coordinate
getValueOfCoordinate :: [[Int]] -> (Int, Int) -> Int
getValueOfCoordinate board (row,col) = (board !! row) !! col

-- Creates a coordinate of form (row, column), converting the letter
-- representation of a column to an Int.
createCoordinate :: [Char] -> (Int, Int)
createCoordinate [letter,num] = ((charToNum num), (convertLetterCoordinateToNum (toUpper letter)))

-- Checks whether the absolute difference between a and b is equal to diff
checkDifference :: (Num a, Eq a) => a -> a -> a -> Bool
checkDifference a b diff = ((a - b) == diff) || ((b - a) == diff)

-- Converts the given character to uppercase.
-- note: borrowed and modified from assignment 2.
toUpper :: Char -> Char
toUpper x
    | x `elem` validLetters = x
    | otherwise = toEnum( fromEnum x - fromEnum 'a' + fromEnum 'A')

-- Converts a character of a digit to an Int
charToNum :: Char -> Int
charToNum c = (fromEnum c) - (fromEnum '0')

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
                              [(intToText i):(rowToSymbol (board !! (i)) ownBoard) | i <- [0..9]]

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
