type GameState = (Board, Color) 
type Move = int 
type Board = [Column] 
data Piece = Empty | Full Color deriving (Show, Eq)
data Winner = Won Player | NotWin Player | TieWin Player deriving (Show, Eq)
type Column = [Pieces] 
data Color = Red | Yellow deriving (Show, Eq)

--Story 3 compute the result of a legal move
updateGame :: GameState -> Move -> GameState
updateGame gs@(board,color) mv = (newBoard, opponentColor color)
    where
        newBoard = (updateBoard mv board color)

updateBoard :: Board -> Move -> Color -> Board
updateBoard 0 [x] color     = (updateColumn x color) :[]
updateBoard 0 (x:xs) color  = (updateColumn x color) :xs
updateBoard mv (x:xs) color = x:updateBoard mv-1 xs color



--splitAt