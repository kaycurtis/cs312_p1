{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Tabl
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.List
import System.Random

-- Boards are 10x10 2D arrays with integers in the range [0,3], where:
-- 0 means the square is empty and has not been hit
-- 1 means a miss 
-- 2 means an unhit ship 
-- 3 means a hit ship 
-- playerboard = replicate 10 (replicate 10 0)
-- aiboard = replicate 10 (replicate 10 0)

empty_square = 0
miss_square = 1
hit_square = 3
unhit_carrier_square = 11
unhit_battleship_square = 12
unhit_cruiser_square = 13
unhit_submarine_square = 14
unhit_destroyer_square = 15

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
        playerboard <- setup 
        printboard playerboard True
        aiboard <- getAiBoard
        play playerboard aiboard True 

play :: [[Int]] -> [[Int]] -> Bool -> IO ()
play playerboard aiboard playerturn =
    do
        if playerturn 
            then do 
                printboard aiboard False
                playertarget <- getTarget aiboard
                newaiboard <- hitTarget aiboard playertarget
                if (allShipsHit newaiboard)
                    then do 
                        putStrLn("Congrats, you win!!")
                else do
                    play playerboard newaiboard False
        else do
            putStrLn("Press a key to have the AI take its turn")
            _ <- getLine
            
            aitarget <- (getAITarget playerboard 0)
            newplayerboard <- hitTarget playerboard aitarget
            
            putStrLn("Here's your board after the AI's turn:")
            printboard newplayerboard True
            
            if (allShipsHit newplayerboard)
                then do
                    putStrLn("Sorry, you lose!")
            else do
                play newplayerboard aiboard True
                
-- Guides the player through selecting a valid target square for their turn.
getTarget :: [[Int]] -> IO (Int, Int)
getTarget aiboard = 
    do
        putStrLn("Please input your target")
        target <- getLine
        if (isValidCoordinate target)
            then do 
                let coordinate = createCoordinate target
                if (validtargetSquare aiboard coordinate) 
                    then return coordinate 
                    else do 
                        putStrLn("You have already hit this coordinate. Please try again")
                        getTarget aiboard
        else do
            putStrLn("Invalid coordinate. Please try again")
            getTarget aiboard

-- Updates the given board to reflect changes necessary from the player
-- selecting the coordinate target.
-- if the target is an unhit ship, changes the value to be a hit ship.
-- if the target is an empty square, changes the value to be a miss.
-- Precondition: the given target returns true in validTargetSquare
hitTarget :: [[Int]] -> (Int, Int) -> IO [[Int]]
hitTarget board target = 
    do 
        if unhitShipAtSquare board target 
            then do 
                putStrLn("It's a HIT!")
                let newboard = updateBoardSquare board target 3
                let shipType = (board !! (fst target)) !! (snd target) 
                if (newShipSunk newboard shipType)
                    then do
                        putStrLn(getShipNameFromNum shipType ++ " has been SUNK!")
                        return (newboard)
                    else
                        return (newboard)
                
            else do
                putStrLn("Miss...")
                return (updateBoardSquare board target 1)
                
-- Returns True if the ship the board contains no unhit squares for the shipType given
newShipSunk :: [[Int]] -> Int -> Bool
newShipSunk board shipType = (foldr (\x count -> length (filter (==shipType) x) + count) 0 board) == 0

-- Returns true if all the ships on the given board are hit, else false.
allShipsHit :: [[Int]] -> Bool
allShipsHit board = 
    (foldr (\x count -> length (filter (==3) x) + count) 0 board) == 17

-- Checks whether the given coordinates are a valid target for a player's turn.
-- That is, returns true if the coordinate is empty or an unhit ship, and returns
-- false if the coordinate has already been hit (i.e. is a hit ship or a miss)
validtargetSquare :: [[Int]] -> (Int, Int) -> Bool
validtargetSquare aiboard coordinate = getValueOfCoordinate aiboard coordinate /= hit_square && getValueOfCoordinate aiboard coordinate /= miss_square

-- Checks whether the given coordinate contains an unhit ship
unhitShipAtSquare :: [[Int]] -> (Int, Int) -> Bool
unhitShipAtSquare aiboard coordinate = getValueOfCoordinate aiboard coordinate `elem` [unhit_battleship_square, unhit_carrier_square, unhit_cruiser_square,
                                                                                        unhit_destroyer_square, unhit_submarine_square]

------------------------------ AI STUFF -------------------------------

-- Gets the target selected by the AI for its turn, given a board
getAITarget :: [[Int]] -> Int -> IO (Int, Int)
getAITarget playerboard row
    | row > 9 = getRandomTarget playerboard
    | otherwise =
        do
            let hitIndices = Data.List.elemIndices 3 (playerboard !! row)
            let targets = getTargetFromHits playerboard hitIndices row
            next <- (getAITarget playerboard (row+1))
            
            return (if (length targets > 0) then (head targets) else next)

-- Gets a list of suitable targets given a list of known hit locations
getTargetFromHits :: [[Int]] -> [Int] -> Int -> [(Int, Int)]
getTargetFromHits playerboard hitIndices row
    | length hitIndices == 0 = []
    | otherwise =  [(i, j) | i <- [row-1, row, row+1], col <- hitIndices, j <- [col-1, col, col+1], isValidCoordinateNum (i, j),
                            ((playerboard !! i) !! j) /= 1 && ((playerboard !! i) !! j) /= 3]
            
-- Gets a random valid target (i.e. not already a hit/miss location)
getRandomTarget :: [[Int]] -> IO (Int, Int)
getRandomTarget playerboard = 
    do
        g <- newStdGen
        return (head [(i, j) | i <- randomRs (0, 9) g, j <- randomRs (0, 9) g,
                                ((playerboard !! i) !! j) /= 1, ((playerboard !! i) !! j) /= 3])

---------------------------- BOARD SETUP ------------------------------

-- Guides the player through placing their ships on their board.
setup :: IO [[Int]]
setup = 
    do
        board1 <- placeShip 5 "Carrier" (replicate 10 (replicate 10 0))
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
                                let newBoard = updateBoardWithShip startCoordinate endCoordinate board size name
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
updateBoardWithShip :: (Int, Int) -> (Int, Int) -> [[Int]] -> Int -> [Char] -> [[Int]]
updateBoardWithShip (sRow, sCol) (eRow, eCol) board size name
    | sRow == eRow = updateRowWithShip board sRow (min sCol eCol) size name
    | sCol == eCol = updateColWithShip board sCol (min sRow eRow) size name

-- Updates the spaces of the board in the given row to contain a ship of
-- size num that starts at column start
updateRowWithShip :: [[Int]] -> Int -> Int -> Int -> [Char] -> [[Int]]
updateRowWithShip board row start num name
    | num == 0 = board
    | otherwise = updateRowWithShip (updateBoardSquare board (row, start) (getShipNumFromName name)) row (start + 1) (num - 1) name

-- Updates the spaces of the board in the given column col to contain a ship of
-- size num that starts at row start
updateColWithShip :: [[Int]] -> Int -> Int -> Int -> [Char] -> [[Int]]
updateColWithShip board col start num name
    | num == 0 = board
    | otherwise = updateColWithShip (updateBoardSquare board (start, col) (getShipNumFromName name)) col (start + 1) (num - 1) name

-- Update square (x,y) on the board to newval
updateBoardSquare :: [[Int]] -> (Int, Int) -> Int -> [[Int]]
updateBoardSquare board (row, col) newval = [[if i == row && j == col then newval else ((board !! i) !! j) | j <- [0..9]] | 
    i <- [0..9]]

-- Checks if the coordinate given is a valid board coordinate.
isValidCoordinate :: [Char] -> Bool
isValidCoordinate [letter, num] = ((toUpper letter) `elem` validLetters) && (num `elem` validNumbers)
isValidCoordinate lst = False

isValidCoordinateNum :: (Int, Int) -> Bool
isValidCoordinateNum (row, col) = row >= 0 && row <= 9 && col >= 0 && col <= 9

-- Checks if the given start and end coordinates are correct for a ship occupying
-- size spaces AND if the spaces between start and end are free.
isValidShipPlacement :: (Int, Int) -> (Int, Int) -> Int -> [[Int]] -> Bool
isValidShipPlacement (sRow, sCol) (eRow, eCol) size board
    | sCol == eCol = checkDifference sRow eRow (size - 1) && isColFreeBetween sRow eRow board sCol
    | sRow ==  eRow = checkDifference sCol eCol (size - 1) && isRowFreeBetween sCol eCol board sRow
    | otherwise = False

-- Return a list of valid end placements, given a start placement
getValidShipPlacements :: (Int, Int) -> Int -> [[Int]] -> [(Int, Int)]
getValidShipPlacements start size board = 
    filter ( \ end -> isValidShipPlacement start end size board) (getValidCoordinatesXAwayFromStart start size)

-- Get a list of coordinates which are <size> away from the start coordinate, which are on the board
getValidCoordinatesXAwayFromStart :: (Int, Int) -> Int -> [(Int, Int)]
getValidCoordinatesXAwayFromStart (sRow, sCol) size =
    let offset = size - 1
    in filter (\ end -> isValidCoordinateNum end) [(sRow, sCol + offset), (sRow, sCol - offset), (sRow + offset, sCol), (sRow - offset, sCol)]

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

-- Gets ship number (on board) from its name
getShipNumFromName :: [Char] -> Int
getShipNumFromName name 
    | name == "Battleship" = unhit_battleship_square
    | name == "Carrier" = unhit_carrier_square
    | name == "Cruiser" = unhit_cruiser_square
    | name == "Destroyer" = unhit_destroyer_square
    | name == "Submarine" = unhit_submarine_square
    
-- Gets ship name from its number (on board)
getShipNameFromNum :: Int -> [Char]
getShipNameFromNum num
    | num == unhit_battleship_square = "Battleship"
    | num == unhit_carrier_square = "Carrier"
    | num == unhit_cruiser_square = "Cruiser"
    | num == unhit_destroyer_square = "Destroyer"
    | num == unhit_submarine_square = "Submarine"

---------------------------- AI BOARD --------------------------------------------

-- board1 = [[0,2,0,0,0,0,0,0,0,0],
--           [0,2,0,0,0,0,0,0,0,0],
--           [0,2,0,0,0,0,0,0,0,0],
--           [0,0,0,0,2,2,2,0,0,0],
--           [0,0,0,0,0,0,0,0,0,0],
--           [0,0,2,2,2,2,0,0,0,0],
--           [0,0,0,0,0,0,0,0,0,0],
--           [0,0,0,0,2,2,2,2,2,0],
--           [0,2,0,0,0,0,0,0,0,0],
--           [0,2,0,0,0,0,0,0,0,0]]

-- Randomly generate a board for the AI
getAiBoard :: IO [[Int]]
getAiBoard = 
    do 
        let board = replicate 10 (replicate 10 0)
        board <- placeOneShip 5 board "Carrier"
        board <- placeOneShip 4 board "Battleship"
        board <- placeOneShip 3 board "Cruiser"
        board <- placeOneShip 3 board "Submarine"
        board <- placeOneShip 2 board "Destroyer"
        putStrLn "Printing the value of the AI board for debugging purposes, delete me!"
        printboard board True -- TEMP, for DEBUGGING ONLY
        return board

-- Randomly place one ship of size size onto the board
placeOneShip :: Int -> [[Int]] -> [Char] -> IO [[Int]]
placeOneShip size board name =
    do 
        generator <- newStdGen
        let lst = take 2 $ (randomRs (0, 9) generator) 
        let start = (lst !! 0, lst !! 1)
        let options = getValidShipPlacements start size board
        let optionsLength = length options
        if optionsLength == 0 
            then do 
                placeOneShip size board name
        else do
            generator <- newStdGen
            let (index, g) = randomR (0, optionsLength - 1) generator
            return (updateBoardWithShip start (options !! index) board size name)

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
    | i == 1    = "X"
    | i `elem` [unhit_battleship_square, unhit_carrier_square, unhit_cruiser_square, unhit_destroyer_square, unhit_submarine_square] = if ownBoard then "☆" else " "   
    | i == 3    = "★"

-- Convert int to T.Text object 
intToText :: Int -> T.Text
intToText i = T.pack (show i)
