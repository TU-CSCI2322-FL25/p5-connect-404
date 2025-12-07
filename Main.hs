{- HLINT ignore "Redundant if" -}
module Main where
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
data Flag = Winner | Depth String | Help | Move String | Verbose | Interactive deriving (Eq, Show)


options :: [OptDescr Flag]
options =
  [ Option ['w'] ["winner"]      (NoArg Winner)          "Prints the best move for a given game",
    Option ['d'] ["depth"]       (ReqArg Depth "<#>")    "Allows the user to specify <#> as a cutoff depth.",
    Option ['h'] ["help"]        (NoArg Help)            "Print usage information and exit.",
    Option ['m'] ["move"]        (ReqArg Move "<#>")     "Makes the move at <#> and returns the resulting board to stdout ",
    Option ['v'] ["verbose"]     (NoArg Verbose)         "Outputs both the move and a description of how good it is: win, lose, tie, or a rating.",
    Option ['i'] ["interactive"] (NoArg Interactive)     "Outputs both the move and a description of how good it is: win, lose, tie, or a rating."

  ]
--story 14: IO functions 
-- main takes file? uses readGame to turn into a GameState then uses BestMove and prints answer
main :: IO ()
main = do
    args <- getArgs
    let opts@(flags, nonFlags, errors) = getOpt Permute options args
    dispatch flags nonFlags errors opts

dispatch flags nonFlags errors opts  |not (null errors) || Help `elem` flags = printHelp opts
                                     |null flags                             = runDepth [Depth "8"] nonFlags -- this is default for story 21
                                     |Winner `elem` flags                    = runWinner nonFlags
                                     |hasDepth flags                         = runDepth flags nonFlags
                                     |hasMove flags                          = runMove flags nonFlags
                                     |hasInteractive flags                   = runInteractive flags nonFlags
                                


fileExists nonFlags = undefined


--flag functions
printHelp :: ([Flag], [String], [String]) -> IO ()
printHelp (flags, inputs, errors) =
  do putStrLn $ show (flags,inputs,errors) 
     putStrLn $ concat errors
     putStrLn $ usageInfo "Fortunes [options] [files]" options


runWinner nonFlags = do
    let file = head nonFlags
    contents <- readFile file
    let
        game = readGame contents
        move = bestMove game
    print move


runDepth flags nonFlags = do 
    let depth = getDepth flags
    --file = head nonFlags 
    --contents <- readFile
    --let
        --game = readGame contents
        --(rating move) = whoMightWin game depth
    --print rating ++ move
    print depth -- this is for test
    

runMove flags nonFlags = do
    let file = head nonFlags
    contents <- readFile file
    let 
        move = getMove flags
        game = readGame contents
        newGame = updateGame game move
        gameString =
            if hasVerbose flags 
            then "rating" ++ "\n"++ (showGame newGame)
            else  showGame newGame
            
    putStrLn gameString

runInteractive :: [Flag] -> [String] -> IO ()
runInteractive flags nonFlags = do
    contents <- readFile $ head nonFlags
    let game = readGame contents
    interactiveLoop flags game

    
-- interactiveLoop :: [Flag] -> GameState -> IO ()
interactiveLoop flags game = do
    putStr "What move do you want? "
    moveStr <- getLine
    let move = read moveStr :: Int
        newGame = updateGame game move
    putStrLn (showGame newGame)
    case gameWinner newGame of
        Nothing -> do
            let newMove   = bestMove newGame
                newerGame = updateGame newGame newMove
            putStrLn (showGame newerGame)
            case gameWinner newerGame of
                Nothing          -> interactiveLoop flags newerGame
                Just winnerColor -> 
                    putStrLn $ "Game over! Winner: " ++ show winnerColor
        Just winnerColor ->
            putStrLn $ "Game over! Winner: " ++ show winnerColor

    




----------------------------------------------


getDepth :: [Flag] -> Int
getDepth [] = 4 --default idk if i shold do this or use a Maybe
getDepth (Depth str:_) = read str
getDepth (_:flags) = getDepth flags

getMove :: [Flag] -> Int
getMove [] = 0 --default idk if i shold do this or use a Maybe
getMove (Move str:_) = read str
getMove (_:flags) = getMove flags


-- i got this from chatgpt but it was the same idea i was trying basically just `elem` for flags with strings
hasDepth :: [Flag] -> Bool
hasDepth = any isDepth
  where
    isDepth (Depth _) = True
    isDepth _         = False

hasMove :: [Flag] -> Bool
hasMove = any isMove
  where
    isMove (Move _) = True
    isMove _         = False

hasVerbose :: [Flag] -> Bool
hasVerbose = any isVerbose
  where
    isVerbose (Verbose) = True
    isVerbose _         = False

hasInteractive :: [Flag] -> Bool
hasInteractive = any isInteractive
  where
    isInteractive (Interactive) = True
    isInteractive _         = False

    
