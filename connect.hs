import Data.List
--story 1
type GameState = (Board, Color) 
type Move = Int 
type Board = [Column] 
data Piece = Empty | Full Color deriving (Show, Eq)
data Winner = Won Color | Ongoing Color | Tie deriving (Show, Eq) 
type Column = [Piece] 
data Color = Red | Yellow deriving (Show, Eq)



--story 2: find the winner
--SHOULD JUST CHECK BOTH PLAYERS
gameWinner :: GameState -> Winner
gameWinner (board, player)
    | isFull board                              = Tie 
    | checkWin board  && (player == Red)        = Won Yellow
    | checkWin board  && (player == Yellow)     = Won Red




--SHOULD RETURN A BOOL!! 
checkWin :: Board -> Bool --could be Won or Ongoing
checkWin board = checkAllColumns board Red || checkAllColumns board Yellow || checkRowsAndDiagonals board Red || checkRowsAndDiagonals board Yellow
 {-    | checkAllColumns board player || checkRowsAndDiagonals board player   = Won player
    | otherwise                                                            = Ongoing player -}
    
    --if any one of these functions return true, then there's a winner
    --checkAllColumns
    --checkRowsAndDiagonals

--checkAllColumns :: Board -> Color -> Bool
checkAllColumns [] player = False
checkAllColumns columns player = 
    let
        checkOneVertical (s1:rest@(s2:s3:s4:_)) = all (==Full player) [s1,s2,s3,s4] || checkOneVertical rest
        checkOneVertical _ = False
                            
    in any checkOneVertical columns

checkRowsAndDiagonals :: Board -> Color -> Bool
checkRowsAndDiagonals (c1:c2:c3:c4:rest) player = 
    or [ checkARow c1 c2 c3 c4
       , checkARow c1 (drop 1 c2) (drop 2 c3) (drop 3 c4) 
       , checkARow (drop 3 c1) (drop 2 c2) (drop 1 c3) c4
       , checkRowsAndDiagonals (c2:c3:c4:rest) player
       ]
        where
            winner = (Full player, Full player, Full player, Full player)
            checkARow c1 c2 c3 c4 = winner `elem` (zip4 c1 c2 c3 c4)
checkRowsAndDiagonals _ player = False
            --goes down a section of 4 columns
--we have two sliding windows: one window moves like c1:c2:c3:c4 -> c2:c3:c4:c5, changing the columns youre looking at.
--the second window slides down each section of four columns. 



isFull :: Board -> Bool --head of all columns are full
isFull board = all (/= Empty) [ head column | column <- board]
-- head (reverse column) --> for if 1st element of col is bottom

--Story 3 compute the result of a legal move

updateGame :: GameState -> Move -> GameState
--possible error handling: what if move is out of bounds for the board?
updateGame gs@(board,color) move = if(move `elem` legalMoves (board,color)) then (newBoard, opponentColor color) else (board,color)
    where
        newBoard = (updateBoard move board color)





updateBoard ::  Move ->  Board -> Color -> Board
updateBoard 0 [x] color     = (updateColumn x color) :[]
updateBoard 0 (x:xs) color  = (updateColumn x color) :xs
updateBoard move (x:xs) color = x:updateBoard (move - 1) xs color

--updateColumn --> Fogarty said to update board from the bottom 


--updateColumn
{- --can we change this so that pieces are added to end of list --> so it's like the actual game 
updateColumn :: Column -> Color -> Column 
updateColumn (Empty:xs) color = Full color : xs
updateColumn (x:xs) color = x : updateColumn xs color -}

--updateColumn --> Fogarty said to update board from the bottom 
--POSSIBLE ERROR CHECK: what if the column is already full?
updateColumn :: Column -> Color -> Column 
updateColumn [x] color = [Full color]
updateColumn (x:y:rest) color
    | y == Full Red  || y == Full Yellow  = Full color:y:rest
    | otherwise                           = x:updateColumn (y:rest) color

--

--updateColumn (E)

--returns opposite color

opponentColor :: Color -> Color
opponentColor Red = Yellow
opponentColor Yellow = Red

--story 4
legalMoves :: GameState -> [Move]
legalMoves (board, color) = [move | (move, Empty:col) <- zip [0..] board]

--story 5

pieceToString :: Piece -> String
pieceToString Empty         = "Empty  "
pieceToString (Full Red)    = "Red    "
pieceToString (Full Yellow) = "Yellow "

-- columnToString :: Column -> String
-- columnToString []     = ""
-- columnToString (x:xs) = pieceToString x ++ columnToString xs

-- boardToString :: Board -> [String]
-- boardToString [] = ""
-- boardToString (x:xs) = columnToString x : boardToString xs

-- Transposes board from column-major to row-major (no !!)

transposeBoard :: Board -> [[Piece]]
transposeBoard board = foldr (zipWith (:)) (replicate 6 []) board

--This function turns the board — which is stored as a list of columns — into a list of rows so it can be printed like a real Connect 4 grid.

-- Pretty-print the board with numbered columns

prettyPrint :: Board -> String
prettyPrint board = unlines (map (concatMap pieceToString) (reverse (transposeBoard board))) ++ " 1      2      3      4      5      6      7 \n"

--for prettyprint you have to do putStrLn (prettyPrint oneFullBoard) for example for it to work)

boardToStringSideways :: String -> String
boardToStringSideways badBoard = undefined

--story 6
checkValidBoard :: Board -> Bool
checkValidBoard board = length [x | x <- board, length x == 6] == 7




----------------------------TESTS----------------------------
--empty column
--board = [[Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty]]

testcolumn = updateColumn emptyColumn Red
testColumn2 = updateColumn littleEmptyColumn Red
testFullMove = updateColumn fullColumn Red


littleEmptyColumn = [Empty, Empty, Empty, Empty, Empty, Full Yellow]

emptyColumn :: Column
emptyColumn = replicate 6 Empty  

--full column
fullColumn :: Column
fullColumn = replicate 6 (Full Red)  

--partial column
partialColumn :: Column
partialColumn = Full Red : replicate 5 Empty  

--partially filled board
partialBoard :: Board
partialBoard = fullColumn : fullColumn : partialColumn : replicate 4 emptyColumn


--Full Board
oneFullBoard :: Board
oneFullBoard = replicate 7 fullColumn

--empty board
emptyBoard :: Board
emptyBoard = replicate 7 emptyColumn  

--gameStart :: GameState
--gameStart = (emptyBoard, Red)

--gameStart2 = updateGame gameStart 4


--testBoard :: GameState
--testBoard = ([[Empty, Empty, Empty ,Empty, Empty, Full Yellow],[Empty, Empty, Full Red, Full Red, Full Red, Full Red],[Empty, Empty, Empty ,Empty, Empty, Full Yellow],[Empty, Empty, Empty, Empty, Empty, Empty],[Empty, Empty, Empty, Empty, Empty, Empty],[Empty, Empty, Empty, Empty, Empty, Empty],[Empty, Empty, Empty ,Empty, Empty, Full Yellow]], Yellow)
testWorkVerticalWin = gameWinner testBoard --expected: WOn red
    where 
        testBoard = ([[Empty, Empty, Empty ,Empty, Empty, Full Yellow],[Empty, Empty, Full Red, Full Red, Full Red, Full Red],[Empty, Empty, Empty ,Empty, Empty, Full Yellow],[Empty, Empty, Empty, Empty, Empty, Empty],[Empty, Empty, Empty, Empty, Empty, Empty],[Empty, Empty, Empty, Empty, Empty, Empty],[Empty, Empty, Empty ,Empty, Empty, Full Yellow]], Yellow)

testWorkHorizontalWin = gameWinner testBoard --expected: won red
    where
        testBoard = ([[Empty, Empty, Empty, Empty, Empty, Empty],[Empty, Empty, Empty, Empty, Full Yellow, Full Yellow],[Empty, Empty, Empty, Empty, Full Yellow, Full Red],[Empty, Empty, Empty, Empty, Full Red, Full Red],[Empty, Empty, Empty, Empty, Full Red, Full Yellow],[Empty, Empty, Empty, Empty, Full Red, Full Yellow], [Empty, Empty, Empty, Empty, Full Red, Full Yellow]], Yellow)

testpleaseWorkDiagonalWin = gameWinner testBoard --expected: won yellow
    where
        testBoard = ([[Empty, Empty, Empty, Empty, Empty, Full Red],[Empty, Full Yellow, Full Red, Full Red, Full Yellow, Full Red],[Empty, Empty, Full Yellow, Full Yellow, Full Yellow, Full Red],[Empty, Empty, Full Yellow, Full Yellow, Full Red, Full Yellow],[Empty, Empty, Empty, Full Red, Full Yellow, Full Red],[Empty, Empty, Empty, Empty, Empty, Empty, Empty],[Empty, Empty, Empty, Empty, Empty, Empty, Empty]], Red)


gameStart :: GameState
gameStart = (emptyBoard, Red)
