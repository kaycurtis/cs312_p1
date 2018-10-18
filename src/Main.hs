{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Tabl
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.List
import System.Random

-- Boards are 10x10 2D arrays with integers where:
-- 0 means the square is empty and has not been hit
-- 1 means a miss 
-- 11- 15 means an unhit ship
-- 3 means a hit ship 
empty_square = 0
miss_square = 1
hit_square = 3
unhit_carrier_square = 11
unhit_battleship_square = 12
unhit_cruiser_square = 13
unhit_submarine_square = 14
unhit_destroyer_square = 15
unhitShipSquares = [unhit_carrier_square, unhit_battleship_square, unhit_cruiser_square,
    unhit_submarine_square, unhit_destroyer_square]

carrier_string = "Carrier"
battleship_string = "Battleship"
cruiser_string = "Cruiser"
submarine_string = "Submarine"
destroyer_string = "Destroyer"

validLetters = ['A' .. 'J']
validNumbers = ['0' .. '9']

-- The main game loop. The user sets up their board, then the AI sets up
-- their board, and then gameplay begins with the human player taking the
-- first turn.
main :: IO ()
main = 
    do
        putStrLn("Welcome to Battleship! Let's get started...")
        putStrLn("First we'll get your board setup.")
        putStrLn("When inputting coordinates, please use the form A1, J9, etc.")
        playerboard <- setupPlayersBoard
        printboard playerboard True
        aiboard <- getAiBoard
        putStrLn("Would you like to see the AI board (for debugging purposes? (Input \"Y\" if so)")
        debug <- getLine
        if ((debug == "Yes") || (debug == "yes") || (debug == "Y") || (debug == "y"))
            then do
                putStrLn("Okay, here is the AI's board:")
                printboard aiboard True
                play playerboard aiboard True
            else do
                putStrLn("Not in debug mode.")
                play playerboard aiboard True 

-- Plays one turn of the game. If playerturn is true, it is the human player's turn.
-- Else, it is the AI's turn. If after the target is selected and hit the game is
-- in the end condition (i.e. all ships have been hit), the game is over and the
-- player whose turn it is wins.
play :: [[Int]] -> [[Int]] -> Bool -> IO ()
play playerboard aiboard playerturn =
    do
        if playerturn 
            then do
                promptToContinue("It's your turn. Press a key to see your opponent's board.")
                printboard aiboard False
                playertarget <- getTarget aiboard
                promptToContinue("Taking aim... Press a key to continue");
                newaiboard <- hitTarget aiboard playertarget
                if (allShipsHit newaiboard)
                    then do 
                        putStrLn("Congrats, you win!!")
                else do
                    play playerboard newaiboard False
        else do
            promptToContinue("Press a key to have the AI take its turn")
            aitarget <- (getAITarget playerboard 0)
            promptToContinue("The AI has selected the target "++(convertNumCoordinateToUserCoordinate aitarget)++". Press a key to continue.")
            newplayerboard <- hitTarget playerboard aitarget
            promptToContinue("Press a key to see your board after the AI's turn")
            printboard newplayerboard True
            
            if (allShipsHit newplayerboard)
                then do
                    putStrLn("Sorry, you lose!")
            else do
                play newplayerboard aiboard True

-- Print the given text and then require the player to press a key
promptToContinue :: [Char] -> IO String
promptToContinue str =
    do
        putStrLn(str)
        x <- getLine
        return x
                
-- Guides the player through selecting a valid target square for their turn.
-- If the user inputs an invalid coordinate or selects a coordinate they have
-- already hit in past turns, recursively call getTarget to try again.
getTarget :: [[Int]] -> IO (Int, Int)
getTarget aiboard = 
    do
        putStrLn("Please input your target")
        target <- getLine
        if (isValidCoordinate target)
            then do 
                let coordinate = createCoordinate target
                if (isUnhitTarget aiboard coordinate) 
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
-- Precondition: the given target returns true in isUnhitTarget
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
                putStrLn("It's a miss...")
                return (updateBoardSquare board target 1)
                
-- Returns True if the board contains no unhit squares for the shipType given, otherwise False.
newShipSunk :: [[Int]] -> Int -> Bool
newShipSunk board shipType = (foldr (\x count -> length (filter (==shipType) x) + count) 0 board) == 0

-- Returns true if all the ships on the given board are hit (ie end of the game), else false.
allShipsHit :: [[Int]] -> Bool
allShipsHit board = 
    (foldr (\x count -> length (filter (==hit_square) x) + count) 0 board) == 17

-- Returns true if the given coordinate has not yet been selected as a target to hit
-- on the given gameboard (i.e. is not a miss or a hit square)
isUnhitTarget :: [[Int]] -> (Int, Int) -> Bool
isUnhitTarget board (i,j) = 
    val /= miss_square && val /= hit_square
    where val = getValueOfCoordinate board (i,j)

-- Checks whether the given coordinate contains an unhit ship
unhitShipAtSquare :: [[Int]] -> (Int, Int) -> Bool
unhitShipAtSquare aiboard coordinate = getValueOfCoordinate aiboard coordinate `elem` unhitShipSquares

------------------------------ AI SPECIFIC FUNCTIONS -------------------------------

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
-- Suitable targets are those that are valid board coordinates, have not yet been hit,
-- and are directly to the left or right OR directly above or below a square already hit.
getTargetFromHits :: [[Int]] -> [Int] -> Int -> [(Int, Int)]
getTargetFromHits board hitIndices row
    | length hitIndices == 0 = []
    | otherwise =  [(i, j) | i <- [row-1, row, row+1], col <- hitIndices, j <- [col-1, col, col+1],
                            isValidCoordinateNum (i, j),
                            isUnhitTarget board (i,j),
                            (i == row-1 && j == col) || (i == row+1 && j == col) || (i == row)]
            
-- Gets a random valid target (i.e. not already a hit/miss location)
getRandomTarget :: [[Int]] -> IO (Int, Int)
getRandomTarget board = 
    do
        g <- newStdGen
        return (head [(i, j) | i <- randomRs (0, 9) g, j <- randomRs (0, 9) g, isUnhitTarget board (i,j)])

---------------------------- BOARD SETUP FUNCTIONS ------------------------------

-- Guides the player through placing their ships on their board.
setupPlayersBoard :: IO [[Int]]
setupPlayersBoard = 
    do
        board1 <- placeShip 5 carrier_string (replicate 10 (replicate 10 0))
        printboard board1 True
        board2 <- placeShip 4 battleship_string board1
        printboard board2 True
        board3 <- placeShip 3 cruiser_string board2
        printboard board3 True
        board4 <- placeShip 3 submarine_string board3
        printboard board4 True
        placeShip 2 destroyer_string board4

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
                if (isFreeSpace board startCoordinate)
                    then do
                        let validEndCoordinates = getValidCoordinatesXAwayFromStartUserFriendly startCoordinate size board
                        putStrLn("Give an end coordinate that is " ++(show (size - 1))++ " spaces away vertically or horizontally from " ++start++ ".")
                        putStrLn("The valid options are: "++(show validEndCoordinates))
                        end <- getLine
                        if ((toUpperUserCoordinate end) `elem` validEndCoordinates)
                            then do
                                let endCoordinate = createCoordinate end
                                let newBoard = updateBoardWithShip startCoordinate endCoordinate board size name
                                return newBoard
                            else do
                                putStrLn("Invalid placement. Either the given end coordinate is improperly formatted, it was an incorrect number of spaces away, or it overlapped another ship already on the board.")
                                putStrLn("Please try again.")
                                placeShip size name board
                    else do
                        putStrLn("Invalid placement. The coordinate given is for a square already containing a ship.")
                        putStrLn("Please try again.")
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

-- Updates the spaces of the board in the given column col to contain a ship of type name
-- and size num that starts at row start
updateColWithShip :: [[Int]] -> Int -> Int -> Int -> [Char] -> [[Int]]
updateColWithShip board col start num name
    | num == 0 = board
    | otherwise = updateColWithShip (updateBoardSquare board (start, col) (getShipNumFromName name)) col (start + 1) (num - 1) name

-- Update square (x,y) on the board to newval
updateBoardSquare :: [[Int]] -> (Int, Int) -> Int -> [[Int]]
updateBoardSquare board (row, col) newval =
    [[if i == row && j == col then newval else getValueOfCoordinate board (i,j) | j <- [0..9]] | i <- [0..9]]

-- Checks if the coordinate given is a valid board coordinate.
isValidCoordinate :: [Char] -> Bool
isValidCoordinate [letter, num] = ((toUpper letter) `elem` validLetters) && (num `elem` validNumbers)
isValidCoordinate lst = False

-- Returns true if the given coordinate in the form (row,col) is on the board (ie valid), else false.
isValidCoordinateNum :: (Int, Int) -> Bool
isValidCoordinateNum (row, col) = row >= 0 && row <= 9 && col >= 0 && col <= 9

-- Checks if the given start and end coordinates are correct for a ship occupying
-- size spaces AND if the spaces between start and end are free.
isValidShipPlacement :: (Int, Int) -> (Int, Int) -> Int -> [[Int]] -> Bool
isValidShipPlacement (sRow, sCol) (eRow, eCol) size board
    | sCol == eCol = checkDifference sRow eRow (size - 1) && isColFreeBetween sRow eRow board sCol
    | sRow ==  eRow = checkDifference sCol eCol (size - 1) && isRowFreeBetween sCol eCol board sRow
    | otherwise = False

-- Return a list of valid (row,col) coordinates that are valid end coordinates for a ship placement,
-- using the given start coordinate.
-- The returned list of coordinates are those that are on the board and will not cause overlap with a
-- ship that has already been placed on the baord.
getValidShipPlacements :: (Int, Int) -> Int -> [[Int]] -> [(Int, Int)]
getValidShipPlacements start size board = 
    filter ( \ end -> isValidShipPlacement start end size board) (getValidCoordinatesXAwayFromStart start size)

-- Get a list of coordinates which are <size> away from the start coordinate, which are on the board
getValidCoordinatesXAwayFromStart :: (Int, Int) -> Int -> [(Int, Int)]
getValidCoordinatesXAwayFromStart (sRow, sCol) size =
    let offset = size - 1
    in filter (\ end -> isValidCoordinateNum end) [(sRow, sCol + offset), (sRow, sCol - offset), (sRow + offset, sCol), (sRow - offset, sCol)]

-- Get a list of coordinates which are <size> sqaures away from <start> that would be valid user input for the
-- corresponding end coordinate
getValidCoordinatesXAwayFromStartUserFriendly :: (Int, Int) -> Int -> [[Int]] -> [[Char]]
getValidCoordinatesXAwayFromStartUserFriendly start size board =
    map convertNumCoordinateToUserCoordinate coordinates
    where coordinates = getValidShipPlacements start size board

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

-- Checks whether the absolute difference between a and b is equal to diff
checkDifference :: (Num a, Eq a) => a -> a -> a -> Bool
checkDifference a b diff = ((a - b) == diff) || ((b - a) == diff)


------------------------- CONVERSION FUNCTIONS ---------------------------------------

-- Converts the given character to uppercase.
-- note: borrowed and modified from assignment 2.
toUpper :: Char -> Char
toUpper x
    | x `elem` validLetters = x
    | otherwise = toEnum( fromEnum x - fromEnum 'a' + fromEnum 'A')

-- Converts the letter in the user input of a coordinate that is validly formatted to uppercase.
toUpperUserCoordinate :: [Char] -> [Char]
toUpperUserCoordinate [letter,number] =
    [toUpper letter, number]
toUpperUserCoordinate lst = lst

-- Takes in a coordinate in the form (row,col) where row and col are Ints, and returns a board
-- coordinate in user-friendly format (e.g. (0,0) = "A0"; (0,1) = "B0" etc.)
convertNumCoordinateToUserCoordinate :: (Int, Int) -> [Char]
convertNumCoordinateToUserCoordinate (row, col) = 
    [(validLetters !! col)]++(show row)

-- Creates a coordinate of form (row, column), converting the letter
-- representation of a column to an Int.
createCoordinate :: [Char] -> (Int, Int)
createCoordinate [letter,num] = ((charToNum num), (convertLetterCoordinateToNum (toUpper letter)))

-- Converts a character of a digit to an Int
charToNum :: Char -> Int
charToNum c = (fromEnum c) - (fromEnum '0')

-- Gets ship number (on board) from its name
getShipNumFromName :: [Char] -> Int
getShipNumFromName name 
    | name == battleship_string = unhit_battleship_square
    | name == carrier_string = unhit_carrier_square
    | name == cruiser_string = unhit_cruiser_square
    | name == destroyer_string = unhit_destroyer_square
    | name == submarine_string = unhit_submarine_square
    
-- Gets ship name from its number (on board)
getShipNameFromNum :: Int -> [Char]
getShipNameFromNum num
    | num == unhit_battleship_square = battleship_string
    | num == unhit_carrier_square = carrier_string
    | num == unhit_cruiser_square = cruiser_string
    | num == unhit_destroyer_square = destroyer_string
    | num == unhit_submarine_square = submarine_string

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

---------------------------- AI BOARD --------------------------------------------

-- Randomly generate a board for the AI
getAiBoard :: IO [[Int]]
getAiBoard = 
    do 
        let board = replicate 10 (replicate 10 0)
        board <- placeOneShip 5 board carrier_string
        board <- placeOneShip 4 board battleship_string
        board <- placeOneShip 3 board cruiser_string
        board <- placeOneShip 3 board submarine_string
        board <- placeOneShip 2 board destroyer_string
        return board

-- Randomly place one ship of size <size> onto the board
-- selects a random coordinate, gets a list of valid ship placements starting at that coordinate (if none, selects another random coordinate),
-- then randomly selects from those placements
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

---------------------------- BOARD PRINTING ------------------------------

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

---------- BOARD PRINTING HELPER METHODS ------------

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
