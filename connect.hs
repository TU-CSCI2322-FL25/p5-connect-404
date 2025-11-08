type GameState = (Board, Color) 
type Move = Int 
type Board = [Column] 
data Piece = Empty | Full Color deriving (Show, Eq)
data Winner = Won Color | NotWin Color | TieWin Color deriving (Show, Eq)
type Column = [Piece] 
data Color = Red | Yellow deriving (Show, Eq)

legalMoves :: GameState -> [Move]
legalMoves gs@(board,color) = [fst x | x <- makeLookupList board, Empty `elem` snd x]


makeLookupList :: Board -> [(Int, Column)]
makeLookupList board = zip [0..6] board
--Story 3 compute the result of a legal move
updateGame :: GameState -> Move -> GameState
updateGame gs@(board,color) move = (newBoard, opponentColor color)
    where
        newBoard = (updateBoard move board color)


updateBoard ::  Move ->  Board -> Color -> Board
updateBoard 0 [x] color     = (updateColumn x color) :[]
updateBoard 0 (x:xs) color  = (updateColumn x color) :xs
updateBoard move (x:xs) color = x:updateBoard (move - 1) xs color

--updateColumn
updateColumn :: Column -> Color -> Column 
updateColumn (Empty:xs) color = Full color : xs
updateColumn (x:xs) color = x : updateColumn xs color

--returns opposite color
opponentColor :: Color -> Color
opponentColor Red = Yellow
opponentColor Yellow = Red

-- splitAt



-- tests stuff 
emptyColumn :: Column
emptyColumn = replicate 6 Empty  

fullColumn :: Column
fullColumn = replicate 6 (Full Red)  

halfFullColumn :: Column
halfFullColumn = Full Red : replicate 5 Empty  

oneFullBoard :: Board
oneFullBoard = fullColumn : fullColumn : halfFullColumn : replicate 4 emptyColumn

gameStart :: GameState
gameStart = (oneFullBoard, Red)
emptyBoard :: Board
emptyBoard = replicate 7 emptyColumn  

gameStart :: GameState
gameStart = (emptyBoard, Red)

gameStart2 = updateGame gameStart 4





