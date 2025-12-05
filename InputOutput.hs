{- HLINT ignore "Redundant if" -}
module InputOutput where
import System.Environment
import Connect
--import Data.Conduit.List (catMaybes)
import Data.Maybe
import System.Console.GetOpt -- for flags

--story 12: readGame function, Y\nYYYRRR\nYYRYYY\n... -> (Board, Color)
readGame :: String -> GameState
readGame str = (stringToBoard str, fromJust (strToColor (head str)))

--stringToBoard :: String -> Board
stringToBoard :: [Char] -> [[Piece]]
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


pieceToStr :: Piece -> Maybe Char
pieceToStr (Full Yellow) = Just 'Y'
pieceToStr (Full Red) = Just 'R'
pieceToStr Empty = Just 'E'
-- story 22-27 flags: 
data Flag = Winner | Depth String | Help deriving (Eq, Show)


options :: [OptDescr Flag]
options =
  [ Option ['w'] ["winner"]    (NoArg Winner)          "print best move for given game",
    Option ['d'] ["depth"]     (ReqArg Depth "<#>")    "Allows the user to specify <num> as a cutoff depth.",
    Option ['h'] ["help"]      (NoArg Help)            "Print usage information and exit."
 
  ]
--story 14: IO functions 
-- main takes file? uses readGame to turn into a GameState then uses BestMove and prints answer
main :: IO ()
main = do
    args <- getArgs
    let opts@(flags, nonFlags, errors) = getOpt Permute options args-- non flags is the file
    if not (null errors) || Help `elem` flags
        then printHelp opts 
       -- else if null nonFlags then
            --putStrLn "no file provided" i thikn the super fancy help i stole should replae this 
        else if Winner `elem` flags 
            then do
                let file = head nonFlags
                contents <- readFile file
                let
                    game = readGame contents
                    move = bestMove game
                print move
        else if  hasDepth flags 
            then do
                let depth = getDepth flags
                print depth -- this is for test
                -- use depth for a funtion that doesnt exist yet and get result and print like winner
                
                
            else putStrLn "Invalid arguments"



-- stolen from fortunes.hs
printHelp :: ([Flag], [String], [String]) -> IO ()
printHelp (flags, inputs, errors) =
  do putStrLn $ show (flags,inputs,errors) 
     putStrLn $ concat errors
     putStrLn $ usageInfo "Fortunes [options] [files]" options



    
getDepth :: [Flag] -> Int
getDepth [] = 4 --default idk if i shold do this or use a Maybe
getDepth (Depth str:_) = read str
getDepth (_:flags) = getDepth flags

-- i got this from chatgpt but it was the same idea i was trying 
hasDepth :: [Flag] -> Bool
hasDepth = any isDepth
  where
    isDepth (Depth _) = True
    isDepth _         = False

    
    {-
main = do
    args <- getArgs
    if null args
        then putStrLn "No file provided"
        else do 
            let file = [a | arg <- args, a <- arg] --hahaahaha...
            contents <- readFile file
            let
                game = readGame contents
                move = bestMove game
            print (move)

    -}
    



    {- main = do
    putStrLn "Enter File Name"
    fileName <- getLine
    contents <- readFile fileName
    let 
        game = readGame contents
        move = undefined :: Move --bestMove game
    print (move) 
    
    -}
