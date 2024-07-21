{- Game logic / win checking -}
module Game(module Game) where

import Data.Maybe
{- Board and counters definition -}
{- In our case, we represent a board as a function from column IDs to lists of tokens.
 - These are sparse and grow upwards. -}
{- Row indices again begin at the bottom left and grow upwards -}
type RowID = Int
type ColumnID = Int
type RowCount = Int
type ColCount = Int
data Player = Red | Yellow
    deriving (Eq)

data Board = MkBoard { board :: [[Player]], numRows :: Int, numCols :: Int }

{- Toggles the current player -}
togglePlayer :: Player -> Player
togglePlayer Red = Yellow
togglePlayer Yellow = Red

{- Board accessors / manipulation function -}

{- Q1(a): emptyBoard -}
emptyBoard :: RowCount -> ColCount -> Board
emptyBoard rs cs = MkBoard {board=replicate cs [], numRows=rs, numCols=cs}

{- Q1(b): getCounter
 - Gets the counter at the given co-ordinates (or Nothing if there is no counter there).
 - Raises an error if co-ordinates are out-of-bounds. -}
getCounter :: Board -> RowID -> ColumnID -> Maybe Player
getCounter b r c =
    if numRows b < r || numCols b < c || r < 0 || c < 0
        then error "Out of bounds!"
    else case length (board b !! c) of
        len | r >= len -> Nothing
        len | otherwise -> Just $ (board b !! c) !! (len - r-1)

{- Q1(c): getRow
 - Retrieves the list of counters on the given row -}
getRow :: Board -> RowID -> [Maybe Player]
getRow b r = [getCounter b r c | c <- [0..numCols b - 1]]


{- Q1(d): getColumn
 - Retrieves the list of counters in the given column, from top-to-bottom -}
getColumn :: Board -> ColumnID -> [Maybe Player]
getColumn b c =
    if numCols b < c || c < 0
        then error "Out of bounds"
    else  [Nothing | _ <- [1..(numRows b - length (board b !! c))-1]] ++ [Just x |  x <- board b !! c]


{- Q2: Show instance -}

{- Show instance for players -}
instance Show Player where
    show p = case p of
        Red -> "R"
        Yellow -> "Y"

{- Instance -}
instance Show Board where
    show b = unlines $ reverse [rowToString (getRow b r) | r <- [0..numRows b - 1]]


rowToString :: [Maybe Player] -> String
rowToString [] = ""
rowToString (Nothing:xs) = "O" ++ rowToString xs
rowToString (Just p:xs) =  case p of
    Red -> "R" ++ rowToString xs
    Yellow -> "Y" ++ rowToString xs


{- Q3: Board update -}

{- Drops a counter into the given column. If the move is legal, returns an updated
 - board. Otherwise returns Nothing. -}
dropCounter :: Board -> ColumnID -> Player -> Maybe Board
dropCounter b c p
  | c >=numCols b || c < 0 = Nothing
  | length (board b !! c) >= numRows b = Nothing
  | otherwise = Just $ newBoardUnsafe b c p

newBoardUnsafe :: Board -> ColumnID -> Player -> Board
newBoardUnsafe b c p =
    let newB = [board b!!col | col <- [0..c-1]] ++ [[p] ++ board b!!c] ++ [board b!!col | col <- [c+1..numCols b - 1]]
    in MkBoard {board = newB, numRows = numRows b, numCols = numCols b}


{- Q4: Diagonals -}
-- Starting from bottom left, get the downwards diagonals for all rows moving up the first column
-- ++ the downwards diagonals starting along the TOP row (apart from the first one which is already in the first list)
getTLBRDiagonals :: Board -> [[Maybe Player]]
getTLBRDiagonals b = [ [getCounter b (startRow-col) col| col <- [0..numCols b-1], col <= startRow] | startRow <- [0..numRows b-1] ]
    ++ [ [getCounter b (numRows b-1+startCol-col) (col)| col <- [startCol..numCols b-1], numRows b-1+startCol-col >= 0] | startCol <- [1..numCols b-1] ]

-- Start from bottom left, get the upwards diagonals for all rows moving up the first column
-- ++ the upwards diagonals starting along the BOTTOM row (apart from the first one which is already in the first list)
getBLTRDiagonals :: Board -> [[Maybe Player]]
getBLTRDiagonals b = [ [getCounter b (col+n) col| col <- [0..numCols b-1], col < numRows b -n] | n <- [0..numRows b-1] ]
                    ++ [ [getCounter b row (row+n)| row <- [0..numRows b-1], row < numCols b -n] | n <- [1..numCols b-1] ]

{- Gets all diagonals from the given board -}


{- Q5: Win checking -}
-- Recursively checks if the given list has a at least 4 elements, and if so, if they are all the same

hasFourInRow :: [Maybe Player] -> Maybe Player
hasFourInRow [] = Nothing
hasFourInRow [_] = Nothing
hasFourInRow [_, _] = Nothing
hasFourInRow [_, _, _] = Nothing
hasFourInRow (a:b:c:d:xs) = if isJust a && a == b && b== c && c == d then a else hasFourInRow (b:c:d:xs)



{- Checks all rows, columns, and diagonals for any subsequences of length 4 -}
checkWin :: Board -> Maybe Player
checkWin b = case mapMaybe hasFourInRow (concat [[getRow b r | r <- [0..numRows b-1]], [getColumn b c | c <- [0..numCols b-1]], getBLTRDiagonals b, getTLBRDiagonals b]) of
    [Red] -> Just Red
    [Yellow] -> Just Yellow
    _ -> Nothing

 
