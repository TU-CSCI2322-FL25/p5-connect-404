module InputOutput where
import System.Environment
import Connect
--import Data.Conduit.List (catMaybes)
import Data.Maybe

--story 12: readGame function, Y\nYYYRRR\nYYRYYY\n... -> (Board, Color)
readGame :: String -> GameState
readGame str = (stringToBoard str, fromJust (strToColor (head str)))

--stringToBoard :: String -> Board
stringToBoard str = [catMaybes [strToPiece piece| piece <- col]| col <- lst]
    where 
        lst = tail $ lines str

strToColor :: Char -> Maybe Color 
strToColor 'Y' = Just Yellow
strToColor 'R' = Just Red
strToColor _ = Nothing


strToPiece :: Char -> Maybe Piece 
strToPiece 'Y' = Just (Full Yellow)
strToPiece 'R' = Just (Full Red)
strToPiece 'E' = Just Empty
strToPiece _ = Nothing

--story 13: showGame function, (Board,Color)
showGame :: GameState -> String 
showGame (board,color) = fromJust(colorToStr color) : "\n" ++ unlines [catMaybes [pieceToStr piece | piece <- col ]|col <- board]  -- This is the part we need to fix! 

colorToStr :: Color -> Maybe Char
colorToStr Yellow = Just 'Y'
colorToStr Red = Just 'R'
colorToStr _ = Nothing

pieceToStr :: Piece -> Maybe Char
pieceToStr (Full Yellow) = Just 'Y'
pieceToStr (Full Red) = Just 'R'
pieceToStr Empty = Just 'E'
pieceToStr _ = Nothing

--story 14: IO functions 
-- main takes file? uses readGame to turn into a GameState then uses BestMove and prints answer
main = do
    args <- getArgs
    if null args
        then putStrLn "No file provided"
        else do 
            let file = [a | arg <- args, a <- arg] --hahaahaha...
            contents <- readFile file
            let
                game = readGame contents
                move = undefined :: Move --bestMove game
            print (move)



    {- main = do
    putStrLn "Enter File Name"
    fileName <- getLine
    contents <- readFile fileName
    let 
        game = readGame contents
        move = undefined :: Move --bestMove game
    print (move) -}

    



