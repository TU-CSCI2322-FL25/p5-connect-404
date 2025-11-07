type GameState = (Board, Color) 
type Move = int 
type Board = [Column] 
data Piece = Empty | Full Color deriving (Show, Eq)
data Winner = Won Player | NoWinner deriving (Show, Eq)
type Column = [Pieces] 
data Color = Red | Yellow deriving (Show, Eq)