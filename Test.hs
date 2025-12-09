module Test where
import Connect
import InputOutput
import Data.Maybe
----------------------------TESTS----------------------------
--these are variables tha make it a lot easier to right boards
y = Full Yellow
r = Full Red
e = Empty
--this is a full board with no winners
--FullBoardNoWinners = 


--this is a board that results in a tie 
tieBoard = [
    [e, r, r, y, y, r],   
    [e, y, r, y, r, y],   
    [e, r, y, r, y, r],   
    [e, r, y, r, y, r],   
    [e, r, y, y, y, r],   
    [e, y, r, y, r, y],   
    [e, r, r, y, r, y]]

redBoard = [
    [e, r, y, r, y, r],   
    [e, y, r, y, r, y],   
    [e, r, y, r, y, r],   
    [e, r, y, r, y, r],   
    [e, r, y, y, y, r],   
    [e, y, r, y, r, y],   
    [e, r, r, y, r, y]]

yellowBoard = [
    [e, r, r, y, r, r],   
    [y, y, r, y, r, y],   
    [y, r, y, r, y, r],   
    [r, r, y, r, y, r],   
    [e, r, y, y, y, r],   
    [e, y, r, y, r, y],   
    [e, r, r, y, r, y]]


----------------------------TESTS----------------------------
--empty column
--board = [[Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty]]

testcolumn = updateColumn emptyColumn Red
testColumn2 = updateColumn littleEmptyColumn Red
testFullMove = updateColumn fullColumn Red


littleEmptyColumn = [Empty, Empty, Empty, Empty, Empty, Full Yellow]

emptyColumn :: Column
emptyColumn = replicate 6 Empty  

--full column
fullColumn :: Column
fullColumn = replicate 6 (Full Red)  

--partial column
partialColumn :: Column
partialColumn = Full Red : replicate 5 Empty  

--partially filled board
partialBoard :: Board
partialBoard = fullColumn : fullColumn : partialColumn : replicate 4 emptyColumn


--Full Board
oneFullBoard :: Board
oneFullBoard = replicate 7 fullColumn

--empty board
emptyBoard :: Board
emptyBoard = replicate 7 emptyColumn  

--gameStart :: GameState
--gameStart = (emptyBoard, Red)

--gameStart2 = updateGame gameStart 4


--testBoard :: GameState
--testBoard = ([[Empty, Empty, Empty ,Empty, Empty, Full Yellow],[Empty, Empty, Full Red, Full Red, Full Red, Full Red],[Empty, Empty, Empty ,Empty, Empty, Full Yellow],[Empty, Empty, Empty, Empty, Empty, Empty],[Empty, Empty, Empty, Empty, Empty, Empty],[Empty, Empty, Empty, Empty, Empty, Empty],[Empty, Empty, Empty ,Empty, Empty, Full Yellow]], Yellow)
testWorkVerticalWin = gameWinner testBoard --expected: won red
    where 
        testBoard = ([[Empty, Empty, Empty ,Empty, Empty, Full Yellow],[Empty, Empty, Full Red, Full Red, Full Red, Full Red],[Empty, Empty, Empty ,Empty, Empty, Full Yellow],[Empty, Empty, Empty, Empty, Empty, Empty],[Empty, Empty, Empty, Empty, Empty, Empty],[Empty, Empty, Empty, Empty, Empty, Empty],[Empty, Empty, Empty ,Empty, Empty, Full Yellow]], Yellow)

testWorkHorizontalWin = gameWinner testBoard --expected: won red
    where
        testBoard = ([[Empty, Empty, Empty, Empty, Empty, Empty],[Empty, Empty, Empty, Empty, Full Yellow, Full Yellow],[Empty, Empty, Empty, Empty, Full Yellow, Full Red],[Empty, Empty, Empty, Empty, Full Red, Full Red],[Empty, Empty, Empty, Empty, Full Red, Full Yellow],[Empty, Empty, Empty, Empty, Full Red, Full Yellow], [Empty, Empty, Empty, Empty, Full Red, Full Yellow]], Yellow)

testpleaseWorkDiagonalWin = gameWinner testBoard --expected: won yellow
    where
        testBoard = ([[Empty, Empty, Empty, Empty, Empty, Full Red],[Empty, Full Yellow, Full Red, Full Red, Full Yellow, Full Red],[Empty, Empty, Full Yellow, Full Yellow, Full Yellow, Full Red],[Empty, Empty, Full Yellow, Full Yellow, Full Red, Full Yellow],[Empty, Empty, Empty, Full Red, Full Yellow, Full Red],[Empty, Empty, Empty, Empty, Empty, Empty, Empty],[Empty, Empty, Empty, Empty, Empty, Empty, Empty]], Red)

testYellowDiagonalWin = gameWinner testBoard
    where
        testBoard = ([[Full Yellow, Full Yellow, Full Yellow, Full Red, Full Red, Full Red],[Full Yellow, Full Yellow, Full Red, Full Yellow, Full Yellow, Full Yellow],[Full Red, Full Yellow, Full Yellow, Full Red, Full Yellow, Full Yellow],[Full Yellow, Full Red, Full Red, Full Red, Full Yellow, Full Red],[Full Red, Full Yellow, Full Red, Full Yellow, Full Red, Full Red],[Full Red, Full Red, Full Yellow, Full Yellow, Full Red, Full Yellow]],Yellow)
gameStart :: GameState
gameStart = (emptyBoard, Red)

testTieGS = ([[Full Yellow, Full Yellow, Full Yellow, Full Red, Full Red, Full Red],[Full Yellow, Full Yellow, Full Red, Full Yellow, Full Yellow, Full Yellow],[Full Red, Full Yellow, Full Yellow, Full Red, Full Yellow, Full Yellow],[Full Yellow, Full Red, Full Red, Full Red, Full Yellow, Full Red],[Full Red, Full Yellow, Full Red, Full Yellow, Full Red, Full Red],[Full Red, Full Red, Full Yellow, Full Yellow, Full Red, Full Yellow]],Yellow)
testTieBoard = [[Full Yellow, Full Yellow, Full Yellow, Full Red, Full Red, Full Red],[Full Yellow, Full Yellow, Full Red, Full Yellow, Full Yellow, Full Yellow],[Full Red, Full Yellow, Full Yellow, Full Red, Full Yellow, Full Yellow],[Full Yellow, Full Red, Full Red, Full Red, Full Yellow, Full Red],[Full Red, Full Yellow, Full Red, Full Yellow, Full Red, Full Red],[Full Red, Full Red, Full Yellow, Full Yellow, Full Red, Full Yellow]]
--problem: testTieBoard should return false for checkRowandDiagonals when it should be false


--expected: false
testDiagonal = [[Empty, Empty, Empty, Empty, Empty, Full Red],[Empty, Empty, Full Red, Full Red, Full Yellow, Full Red],[Empty, Empty, Full Yellow, Full Yellow, Full Yellow, Full Red],[Empty, Empty, Full Yellow, Full Yellow, Full Red, Full Yellow],[Empty, Empty, Empty, Full Red, Full Yellow, Full Red],[Empty, Empty, Empty, Empty, Empty, Empty, Empty],[Empty, Empty, Empty, Empty, Empty, Empty, Empty]]

shouldBeTie = [[Full Red, Full Red, Full Yellow, Full Red, Full Yellow, Full Yellow], [Full Yellow, Full Red, Full Red, Full Yellow, Full Yellow, Full Yellow], [Full Yellow, Full Red, Full Red, Full Yellow, Full Red, Full Red], [Full Yellow, Full Yellow, Full Red, Full Yellow, Full Red, Full Red], [Full Red, Full Yellow, Full Yellow, Full Red, Full Yellow, Full Yellow], [Full Red, Full Red, Full Yellow, Full Red, Full Yellow, Full Yellow], [Full Yellow, Full Red, Full Red, Full Red, Full Yellow, Full Yellow]]


--expected (1000, Nothing)
testDiagonalWin = ([[Empty, Empty, Empty, Empty, Empty, Empty],[Empty, Empty, Empty, Empty, Full Yellow, Full Yellow],[Empty, Empty, Empty, Empty, Full Yellow, Full Red],[Empty, Empty, Empty, Empty, Full Red, Full Red],[Empty, Empty, Empty, Empty, Full Red, Full Yellow],[Empty, Empty, Empty, Empty, Full Red, Full Yellow], [Empty, Empty, Empty, Empty, Full Red, Full Yellow]], Yellow)

testGreatRedPosition = ([[Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Empty], [Empty, Empty, Empty, Empty, Empty, Full Yellow], [Empty, Empty, Empty, Full Red, Full Red, Full Red], [Empty, Empty, Empty, Empty, Empty, Full Yellow], [Empty, Empty, Empty, Empty, Empty, Full Yellow], [Empty, Empty, Empty, Empty, Empty, Empty]], Yellow)