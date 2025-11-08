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

columnToString :: Column -> String
columnToString []     = ""
columnToString (x:xs) = pieceToString x ++ columnToString xs

boardToString :: Board -> String
boardToString [] = ""
boardToString (x:xs) = columnToString x ++ "\n" ++ boardToString xs




emptyColumn :: Column
emptyColumn = replicate 6 Empty  

fullColumn :: Column
fullColumn = replicate 6 (Full Red)  

halfFullColumn :: Column
halfFullColumn = Full Red : replicate 5 Empty  

oneFullBoard :: Board
oneFullBoard = fullColumn : fullColumn : halfFullColumn : replicate 4 emptyColumn

emptyBoard :: Board
emptyBoard = replicate 7 emptyColumn  

gameStart :: GameState
gameStart = (emptyBoard, Red)

