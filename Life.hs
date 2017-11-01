import System.Environment
import Data.List
import Data.Char (chr)
import qualified Data.Set as Set
import Control.Monad

type Cell = (Int, Int)

type Board = (Int, [Cell])

createBoard :: Int -> Board 
createBoard n = (n, [])

neighboursOfCell :: Cell -> Int -> [Cell]
neighboursOfCell cell@(i, j) n = 
  [(i', j') | (i', j') <- neighbours, 0 <= i', i' < n, 0 <= j', j' < n]
    where neighbours = map (\(x, y) -> (i + x, j + y)) delta
          delta = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

deadNeighboursOfCell :: [Cell] -> Int -> Cell -> [Cell]
deadNeighboursOfCell live n cell = filter (\c -> notElem c live) $ neighboursOfCell cell n

aliveNeighboursOfCell :: [Cell] -> Int -> Cell -> [Cell]
aliveNeighboursOfCell live n cell = filter (\c -> elem c live) $ neighboursOfCell cell n

deadlist :: Board -> [Cell] 
deadlist (n, live) = 
  let lst = map (deadNeighboursOfCell live n) live in
  let dead = foldl (\dead deadList -> Set.union dead $ Set.fromList deadList) Set.empty lst 
  in Set.toList dead

evolution :: Board -> Board
evolution board@(n, live) = 
  let ok c = (if (elem c live) 
                then (nbAliveNeighbours c) `elem` [2, 3]
                else (nbAliveNeighbours c) == 3)
      nbAliveNeighbours c = length $ aliveNeighboursOfCell live n $ c 
      dead = deadlist board 
  in
  (n, [c | c <- live ++ dead, ok c])

printBoard :: Board -> IO ()
printBoard board@(n, live) = do
  mapM_ (\x -> do
    mapM_ (\y -> putChar $ (if (x,y) `elem` live then '@' else 'O')) [0..(n-1)]
    putChar '\n'
    ) [0..(n-1)]

simulation :: Board -> Int -> IO ()
simulation board k = do
  case k of 
    0 -> return ()
    _ -> do 
           printBoard board 
           putChar '\n'
           putChar '\n'
           simulation (evolution board) (k-1)

main = do
    (file:_) <- getArgs
    allLines <- liftM lines $ readFile $ file
    let n = read . head $ allLines
    let live = map (\(x:y:_) -> (read x, read y)) (map words $ tail allLines)
    putStrLn "Game of life"
    putStrLn "Number of steps of simulation: "
    k <- getLine
    simulation (n, live) (read k)

