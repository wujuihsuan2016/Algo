import Data.List

type Cell = (Int, Int)
type Board = ([Cell], Cell, Direction)  -- (Black cells, position of the ant, direction)

data Direction = N | E | S | W

printBoard :: Board -> Int -> IO ()
printBoard board@(blackCells, currCell, _) n = do
  mapM_ (\i -> do
    mapM_ (\j -> putChar $ (if (i,j) == currCell
                               then if elem (i,j) blackCells then 'A' else 'a'
                               else if elem (i,j) blackCells then '#' else '.')) [-n..n]
    putChar '\n'
    ) [-n..n]
  putChar '\n'

nextCell :: Cell -> Direction -> Cell
nextCell (i, j) dir = case dir of
  N -> (i, j + 1)
  E -> (i + 1, j)
  S -> (i, j - 1)
  W -> (i - 1, j)

left :: Direction -> Direction
left N = W
left E = N
left S = E
left W = S

right :: Direction -> Direction
right N = E
right E = S
right S = W
right W = N

simulation :: Board -> Board
simulation (blackCells, currCell, dir) 
  | elem currCell blackCells = 
      let newdir = left dir in
      (delete currCell blackCells, nextCell currCell newdir, newdir)
  | otherwise = 
      let newdir = right dir in
      (currCell:blackCells, nextCell currCell newdir, newdir)

simulationLandon :: Board -> Int ->  Int -> IO ()
simulationLandon board n k = case k of
  0 -> return ()
  otherwise -> do
    printBoard board n
    simulationLandon (simulation board) n (k-1)

initBoard :: Board
initBoard = ([], (0,0), N)

main :: IO ()
main = do
  putStrLn "Number of round:" 
  k <- getLine
  putStrLn "Size of the board: (the board will be {(i,j) | -n <= i,j <= n})"
  n <- getLine
  let board = initBoard
  simulationLandon board (read n) (read k)
