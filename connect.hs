type GameState = (Board, Color) 
type Move = Int 
type Board = [Column] 
data Piece = Empty | Full Color deriving (Show, Eq)
data Winner = Won Color | NotWin Color | TieWin Color deriving (Show, Eq)
type Column = [Piece] 
data Color = Red | Yellow deriving (Show, Eq)


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

--for prettyprint you have to do putStrLn (preetyPrint oneFullBoard) for example for it to work)

boardToStringSideways :: String -> String
boardToStringSideways badBoard = undefined



--empty column
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

gameStart :: GameState
gameStart = (emptyBoard, Red)
