module Connect where
import Data.List
import Data.Maybe
import Control.Monad (when)

type GameState = (Board, Color) 
type Move = Int 
type Board = [Column] 
data Piece = Empty | Full Color deriving (Show, Eq)
data Winner = Won Color | Tie deriving (Show, Eq) 
type Column = [Piece] 
data Color = Red | Yellow deriving (Show, Eq)



--story 2: find the winner
--SHOULD JUST CHECK BOTH PLAYERS
gameWinner :: GameState -> Maybe Winner
gameWinner (board, player) 
    | checkWin board Yellow  = Just (Won Yellow)
    | checkWin board Red     = Just (Won Red)
    | isFull board           = Just Tie
    | otherwise              = Nothing

{- Problems with current gameWinner 
It still depends on player, which makes your result wrong depending on whose turn it is.

Example of the bug:

Yellow just won

But player == Red
→ your code incorrectly says Red won Yellow’s victory.

Correct Version: gameWinner :: GameState -> Maybe Winner
gameWinner (board, _) 
    | checkWin board Red    = Just (Won Red)
    | checkWin board Yellow = Just (Won Yellow)
    | isFull board          = Just Tie
    | otherwise             = Nothing

Checks both players independently
Never relies on turn order
Returns Nothing for ongoing games
Story 8 is training  for minimax recursion (Story 9).

Minimax needs:
    a function that says “this state is terminal, here is the winner”
    OR returns Nothing if you must continue recursing


-}


--SHOULD RETURN A BOOL!! 
checkWin :: Board -> Color -> Bool --could be Won or Ongoing
checkWin board player = checkAllColumns board player || checkRowsAndDiagonals board player
 {-    | checkAllColumns board player || checkRowsAndDiagonals board player   = Won player
    | otherwise                                                            = Ongoing player -}
    
    --if any one of these functions return true, then there's a winner
    --checkAllColumns
    --checkRowsAndDiagonals

--checkAllColumns :: Board -> Color -> Bool
checkAllColumns [] player = False
checkAllColumns columns player = 
    let
        checkOneVertical (s1:rest@(s2:s3:s4:_)) = all (==Full player) [s1,s2,s3,s4] || checkOneVertical rest
        checkOneVertical _ = False

    in any checkOneVertical columns

checkRowsAndDiagonals :: Board -> Color -> Bool
checkRowsAndDiagonals (c1:c2:c3:c4:rest) player = 
    or [ checkARow c1 c2 c3 c4
       , checkARow c1 (drop 1 c2) (drop 2 c3) (drop 3 c4) 
       , checkARow (drop 3 c1) (drop 2 c2) (drop 1 c3) c4
       , checkRowsAndDiagonals (c2:c3:c4:rest) player
       ]
        where
            winner = (Full player, Full player, Full player, Full player)
            checkARow c1 c2 c3 c4 = winner `elem` (zip4 c1 c2 c3 c4)
checkRowsAndDiagonals _ player = False
            --goes down a section of 4 columns
--we have two sliding windows: one window moves like c1:c2:c3:c4 -> c2:c3:c4:c5, changing the columns youre looking at.
--the second window slides down each section of four columns. 



isFull :: Board -> Bool --head of all columns are full
isFull board = all (/= Empty) [ head column | column <- board]
-- head (reverse column) --> for if 1st element of col is bottom

--Story 3 compute the result of a legal move

updateGame :: GameState -> Move -> GameState
--possible error handling: what if move is out of bounds for the board?
updateGame gs@(board,color) move = 
    if(move `elem` legalMoves gs) 
        then if(checkValidBoard newBoard) 
            then(newBoard, opponentColor color) 
            else gs 
        else error "illegal move dont do that pls"
    where
        newBoard = (updateBoard move board color)





updateBoard ::  Move ->  Board -> Color -> Board
updateBoard 0 (col:cols) color    = (updateColumn col color):cols
updateBoard move (col:cols) color = col:updateBoard (move - 1) cols color
updateBoard _ _ _        = error "something has gone terribly wrong"
--updateColumn --> Fogarty s to update board from the bottom 


--updateColumn
{- --can we change this so that pieces are added to end of list --> so it's like the actual game 
updateColumn :: Column -> Color -> Column 
updateColumn (Empty:xs) color = Full color : xs
updateColumn (x:xs) color = x : updateColumn xs color -}

--updateColumn --> Fogarty said to update board from the bottom 
--POSSIBLE ERROR CHECK: what if the column is already full?
updateColumn :: Column -> Color -> Column 
updateColumn (Empty:Empty:rest) color = Empty:(updateColumn (Empty:rest)) color
updateColumn (Empty:rest)       color =  (piece) :rest
    where
        piece = if color == Red then Full Red else Full Yellow
--

--updateColumn (E)

--returns opposite color

opponentColor :: Color -> Color
opponentColor Red = Yellow
opponentColor Yellow = Red

--story 4
legalMoves :: GameState -> [Move]
legalMoves (board, color) = [move | (move, Empty:col) <- zip [0..] board]

--story 5

pieceToString :: Piece -> String
pieceToString Empty         = "Empty  "
pieceToString (Full Red)    = "Red    "
pieceToString (Full Yellow) = "Yellow "

-- columnToString :: Column -> String
-- columnToString []     = ""
-- columnToString (x:xs) = pieceToString x ++ columnToString xs

-- boardToString :: Board -> [String]
-- boardToString [] = ""
-- boardToString (x:xs) = columnToString x : boardToString xs

-- Transposes board from column-major to row-major (no !!)

transposeBoard :: Board -> [[Piece]]
transposeBoard board = if(checkValidBoard board) then foldr (zipWith (:)) (replicate 6 []) board else error "bad board"

--for prettyprint you have to do putStrLn (prettyPrint oneFullBoard) for example for it to work)

--story 6
checkValidBoard :: Board -> Bool
checkValidBoard board = length [x | x <- board, length x == 6] == 7

isValidMove :: Move -> GameState -> Bool
isValidMove n game = n `elem` legalMoves game 


--story9branch
whoWillWin :: GameState -> Winner
whoWillWin gs@(board,color) = case gameWinner gs of
    Nothing -> chooseMove listOfFutureGame color
    Just x  -> x
    where
         listOfFutureGame = [updateGame gs x | x <- legalMoves gs] 


chooseMove :: [GameState] -> Color -> Winner
chooseMove futures color =  if Won color `elem` futureWins 
                            then Won color 
                            else if Tie `elem` futureWins 
                            then Tie 
                            else Won (opponentColor color)
                            where
                                futureWins = [whoWillWin x | x <- futures]


--story 10
bestMove :: GameState -> Move
bestMove game@(board,color) = 
    if null legalList 
        then error "BLOW UP AHHHHH" 
        else
            case lookup (Won color) lookUpList of
                Just x ->  x
                Nothing -> 
                    case lookup Tie lookUpList of
                        Just x ->  x
                        Nothing -> snd $ head lookUpList
    where
        lookUpList = [(whoWillWin (updateGame game x),x) | x <- legalList]
        filterForWin win = filter(\x -> snd x == win) lookUpList
        legalList = legalMoves game




