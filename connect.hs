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