module InputOutput where
import Connect

--story 12: readGame function, Y\nYYYRRR\nYYRYYY\n... -> (Board, Color)
readGame :: String -> GameState
readGame str = (stringToBoard str, strToColor (head str))

stringToBoard :: String -> Board
stringToBoard str = [[strToPiece piece | piece <- col]| col <- lst]
    where 
        lst = tail $ lines str

strToColor :: Char -> Color 
strToColor 'Y' = Yellow
strToColor 'R' = Red

strToPiece :: Char -> Piece 
strToPiece 'Y' = Full Yellow
strToPiece 'R' = Full Red
strToPiece 'E' = Empty

--story 13: showGame function, (Board,Color)
showGame :: GameState -> String 
showGame (board,color) = colorToStr color : "\n" ++ unlines [[pieceToStr piece | piece <- col ]|col <- board]  -- This is the part we need to fix! 

colorToStr :: Color -> Char
colorToStr Yellow = 'Y'
colorToStr Red = 'R'

pieceToStr :: Piece -> Char
pieceToStr (Full Yellow) = 'Y'
pieceToStr (Full Red) = 'R'
pieceToStr Empty = 'E'

--story 14: IO functions 
-- main takes file? uses readGame to turn into a GameState then uses BestMove and prints answer
main = do
    putStrLn "Enter File Name"
    fileName <- getLine 
    contents <- readFile fileName
    let 
        game = readGame contents
        move = undefined :: Move --bestMove game
    print (move)

    



