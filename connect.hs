-- Data GameState state = (Board, )
Data Player = Red | Yellow
Data Move = (Piece, Player, Row)
Data Board = [Row]
Data Pieces = Empty | Red | Yellow
Data Winner = Player | NoWinner
Data Row = [Pieces]