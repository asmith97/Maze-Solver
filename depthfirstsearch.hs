import System.IO
import Data.List
import System.Environment
data Tree a = None [(a,a)]| Leaf (a,a) [(a,a)]| Node (a,a) (Tree a) (Tree a) (Tree a) (Tree a) [(a,a)] deriving Show

--This problem was inspired by the hackerrank.com Pacman searching algorithim exercises


--modifiy to allow it to solve multiple boards with multiple start positions
solveMult :: Position -> [Board] -> [[Position]]
solveMult startTuple boards = fmap bestPath (fmap (\b -> buildTree (None [startTuple]) startTuple b)  boards)

--TODO: SOLVE EACH BOARD FILE PASSED IN
main :: IO ()
main = do
    args <- getArgs
    {-board <- readFile (args !! 0)
    putStr "Input the starting coordinates (x,y): "
    hFlush stdout
    startCoord <- getLine
    --check to make sure that the starting tuple is in a '-' or at the '.'
    let startTuple = read startCoord :: (Int, Int)
    let boardArray = lines board
    let yMax = length boardArray
    let xMax = length (boardArray !! 0)
    if fst startTuple < xMax && snd startTuple < yMax then 
        if ((boardArray !! snd startTuple) !! fst startTuple) /= '%' then
            do 
                let tree = buildTree (None [startTuple]) startTuple boardArray
                let best = bestPath tree
                putStrLn board
                if null best then putStrLn "No winning path" else putStrLn $ show best
        else
            putStrLn "The inputted location is in a wall"
    else
        putStrLn "The inputted coordinates are not valid on the board"-}
    boards <- mapM readFile args
    let bs = fmap lines boards
    putStr "Input the starting coordinates (x,y): "
    hFlush stdout
    startCoord <- getLine
    let posTuple = (read startCoord :: (Int, Int))
    let validPos = check posTuple bs
    if not validPos then
            putStrLn "The inputted coordinates are not valid on the board or are in a wall"
    else
        do
            let positions = solveMult posTuple bs
            mapM_ (putStrLn . printSolution) positions

printSolution :: [Position] -> String
printSolution [] = "No solution given this initial value"
--getting rid of the first element becuase it got double counted
printSolution (a:as) = show as


check :: Position -> [Board] -> Bool
check _ [] = True
check pos@(x,y) (a:as)
    | x < xMax && y < yMax && ((a !! y) !! x) /= '%' = check pos as
    | otherwise = False
        where
            yMax = length a
            xMax = length (a !! 0)

type Board = [String]
type Position = (Int, Int)

getValidNeighbors :: Position -> Board -> [Position]
--valid when x, y > 0 and less than the board dimensions (in the middle)
getValidNeighbors (x,y) board = [(a,b) | (a,b) <- [(x, y+1), (x, y-1), (x+1, y), (x-1, y)], ((board !! b) !! a) /= '%']

board = ["%%%%%%%%%%%%%%%%%%%%", "%--------------%---%"
            , "%-%%-%%-%%-%%-%%-%-%", "%----------------%-%", "%%%%%%%%%%%%%%%%%%-%"
            , "%.-----------------%", "%%%%%%%%%%%%%%%%%%%%"]

board' = ["%%%%%%%%%%%%%%%%%%%%", "%-------------.%---%"
            , "%%%%%%%%%%%%%%%%%%%%"]

board'' = ["%%%%", "%--.%"
            , "%%%%"]

isTarget :: Position -> Board -> Bool
isTarget (x,y) board = ((board !! y) !! x) == '.'

buildTree :: Tree Int -> Position -> Board -> Tree Int
buildTree (None previousPos) (x,y) board
    | reachedTarget = Leaf (otherN !! 0) updatedPos
    | len == 0 = Node (x,y) (None updatedPos) (None updatedPos) (None updatedPos) (None updatedPos) updatedPos
    | len == 1 = Node (x,y) (buildTree (None updatedPos) (neighbors !! 0) board) (None updatedPos) (None updatedPos) (None updatedPos) (previousPos ++ [(x,y)])
    | len == 2 = Node (x,y) (buildTree (None updatedPos) (neighbors !! 0) board) (buildTree (None updatedPos) (neighbors !! 1) board) (None updatedPos) (None updatedPos) (previousPos ++ [(x,y)])
    | len == 3 = Node (x,y) (buildTree (None updatedPos) (neighbors !! 0) board) (buildTree (None updatedPos) (neighbors !! 1) board) (buildTree (None updatedPos) (neighbors !! 2) board) (None updatedPos) (previousPos ++ [(x,y)])
    | len == 4 = Node (x,y) (buildTree (None updatedPos) (neighbors !! 0) board) (buildTree (None updatedPos) (neighbors !! 1) board) (buildTree (None updatedPos) (neighbors !! 2) board) (buildTree (None updatedPos) (neighbors !! 3) board) (previousPos ++ [(x,y)])
    where 
        neighbors' = getValidNeighbors (x,y) board
        len = length neighbors
        neighbors = filter (\x -> not $ elem x previousPos) neighbors'
        updatedPos = previousPos ++ [(x,y)]
        --bad hack, but if the valid neighbor is the target, add it to the end of neighbors
        --again and then make this a leaf
        --only works (if it does at all if there is ONE target)
        otherN = (filter (\pos -> isTarget pos board) neighbors)
        reachedTarget = length otherN == 1

leafLengthList :: Tree a -> [Int]
leafLengthList (None _) = []
leafLengthList (Leaf _ list) = [length list]
leafLengthList (Node _ a b c d _) = leafLengthList a ++ leafLengthList b ++ leafLengthList c ++ leafLengthList d

bestPath :: Tree Int -> [Position]
bestPath (None _) = []
bestPath (Leaf _ list) = list
--want to always pick the branch that has the lowest leafLength
bestPath (Node _ a b c d _)
    | la == smallest = bestPath a
    | lb == smallest = bestPath b
    | lc == smallest = bestPath c
    | ld == smallest = bestPath d
    where
        la = sort $ leafLengthList a
        lb = sort $ leafLengthList b
        lc = sort $ leafLengthList c
        ld = sort $ leafLengthList d
        filtered = filter (\x -> not $ x == []) [la, lb, lc, lb]
        smallest = if null filtered then [] else minimum filtered

--some sample boards for testing
b = ["%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    , "%-------%-%-%-----------%---%-----%-%"
    , "%-%%%%%%%-%-%%%-%-%%%-%%%-%%%%%%%-%-%"
    , "%-------%-------%-%-----%-----%-%---%"
    , "%%%%%-%%%%%-%%%-%-%-%-%%%-%%%%%-%-%%%"
    , "%---%-%-%-%---%-%-%-%---%-%---%-%---%"
    , "%-%%%-%-%-%-%%%-%%%%%-%%%-%-%%%-%%%-%"
    , "%-------%-----%---%---%-----%-%-%---%"
    , "%%%-%%%%%%%%%-%%%%%%%-%%%-%%%-%-%-%-%"
    , "%-------------%-------%-%---%-----%-%"
    , "%-%-%%%%%-%-%%%-%-%-%%%-%-%%%-%%%-%-%"
    , "%-%-%-----%-%-%-%-%-----%---%-%-%-%-%"
    , "%-%-%-%%%%%%%-%-%%%%%%%%%-%%%-%-%%%-%"
    , "%-%-%-%-----%---%-----%-----%---%---%"
    , "%%%-%%%-%-%%%%%-%%%%%-%%%-%%%-%%%%%-%"
    , "%-----%-%-%-----%-%-----%-%---%-%-%-%"
    , "%-%-%-%-%-%%%-%%%-%%%-%%%-%-%-%-%-%-%"
    , "%-%-%-%-%-----------------%-%-%-----%"
    , "%%%-%%%%%%%-%-%-%%%%%-%%%-%-%%%-%%%%%"
    , "%-------%-%-%-%-----%---%-----%-%---%"
    , "%%%%%-%-%-%%%%%%%%%-%%%%%%%%%%%-%-%%%"
    , "%---%-%-----------%-%-----%---%-%---%"
    , "%-%%%-%%%%%-%%%%%%%%%-%%%%%-%-%-%%%-%"
    , "%-%---%------%--------%-----%-------%"
    , "%-%-%-%%%%%-%%%-%-%-%-%-%%%%%%%%%%%%%"
    , "%-%-%---%-----%-%-%-%-------%---%-%-%"
    , "%-%-%%%-%%%-%-%-%-%%%%%%%%%-%%%-%-%-%"
    , "%-%---%-%---%-%-%---%-%---%-%-%-----%"
    , "%-%%%-%%%-%%%%%-%%%-%-%-%%%%%-%-%%%%%"
    , "%-------%---%-----%-%-----%---%-%---%"
    , "%%%-%-%%%%%-%%%%%-%%%-%%%-%-%%%-%-%%%"
    , "%-%-%-%-%-%-%-%-----%-%---%-%---%-%-%"
    , "%-%-%%%-%-%-%-%-%%%%%%%%%-%-%-%-%-%-%"
    , "%---%---%---%-----------------%-----%"
    , "%-%-%-%-%%%-%%%-%%%%%%%-%%%-%%%-%%%-%"
    , "%.%-%-%-------%---%-------%---%-%---%"
    , "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"]



b' = ["%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    , "%-------%-%-%-----------%---%-----%-%"
    , "%-%%%%%%%-%-%%%-%-%%%-%%%-%%%%%%%-%-%"
    , "%-------%-------%-%-----%-----%-%---%"
    , "%%%%%-%%%%%-%%%-%-%-%-%%%-%%%%%-%-%%%"
    , "%---%-%-%-%---%-%-%-%---%-%---%-%---%"
    , "%-%%%-%-%-%-%%%-%%%%%-%%%-%-%%%-%%%-%"
    , "%-------%-----%---%---%-----%-%-%---%"
    , "%%%-%%%%%%%%%-%%%%%%%-%%%-%%%-%-%-%-%"
    , "%-------------%-------%-%---%-----%-%"
    , "%-%-%%%%%-%-%%%-%-%-%%%-%-%%%-%%%-%-%"
    , "%-%-%-----%-%-%-%-%-----%---%-%-%-%-%"
    , "%-%-%-%%%%%%%-%-%%%%%%%%%-%%%-%-%%%-%"
    , "%-%-%-%-----%---%-----%-----%---%---%"
    , "%%%-%%%-%-%%%%%-%%%%%-%%%-%%%-%%%%%-%"
    , "%-----%-%-%-----%-%-----%-%---%-%-%-%"
    , "%-%-%-%-%-%%%-%%%-%%%-%%%-%-%-%-%-%-%"
    , "%-%-%-%-%-----------------%-%-%-----%"
    , "%%%-%%%%%%%-%-%-%%%%%-%%%-%-%%%-%%%%%"
    , "%-------%-%-%-%-----%---%-----%-%---%"
    , "%%%%%-%-%-%%%%%%%%%-%%%%%%%%%%%-%-%%%"
    , "%%%-%%%%%%%%%-%%%%%%%-%%%-%%%-%-%-%-%"
    , "%-------------%-------%-%---%-----%-%"
    , "%-%-%%%%%-%-%%%-%-%-%%%-%-%%%-%%%-%-%"
    , "%-%-%-----%-%-%-%-%-----%---%-%-%-%-%"
    , "%-%-%-%%%%%%%-%-%%%%%%%%%-%%%-%-%%%-%"
    , "%-%-%-%-----%---%-----%-----%---%---%"
    , "%%%-%%%-%-%%%%%-%%%%%-%%%-%%%-%%%%%-%"
    , "%-----%-%-%-----%-%-----%-%---%-%-%-%"
    , "%-%-%-%-%-%%%-%%%-%%%-%%%-%-%-%-%-%-%"
    , "%-%-%-%-%-----------------%-%-%-----%"
    , "%%%-%%%%%%%-%-%-%%%%%-%%%-%-%%%-%%%%%"
    , "%-------%-%-%-%-----%---%-----%-%---%"
    , "%%%%%-%-%-%%%%%%%%%-%%%%%%%%%%%-%-%%%"
    , "%%%-%%%%%%%%%-%%%%%%%-%%%-%%%-%-%-%-%"
    , "%-------------%-------%-%---%-----%-%"
    , "%-%-%%%%%-%-%%%-%-%-%%%-%-%%%-%%%-%-%"
    , "%-%-%-----%-%-%-%-%-----%---%-%-%-%-%"
    , "%-%-%-%%%%%%%-%-%%%%%%%%%-%%%-%-%%%-%"
    , "%-%-%-%-----%---%-----%-----%---%---%"
    , "%%%-%%%-%-%%%%%-%%%%%-%%%-%%%-%%%%%-%"
    , "%-----%-%-%-----%-%-----%-%---%-%-%-%"
    , "%-%-%-%-%-%%%-%%%-%%%-%%%-%-%-%-%-%-%"
    , "%-%-%-%-%-----------------%-%-%-----%"
    , "%%%-%%%%%%%-%-%-%%%%%-%%%-%-%%%-%%%%%"
    , "%-------%-%-%-%-----%---%-----%-%---%"
    , "%%%%%-%-%-%%%%%%%%%-%%%%%%%%%%%-%-%%%"
    , "%%%-%%%%%%%%%-%%%%%%%-%%%-%%%-%-%-%-%"
    , "%-------------%-------%-%---%-----%-%"
    , "%-%-%%%%%-%-%%%-%-%-%%%-%-%%%-%%%-%-%"
    , "%-%-%-----%-%-%-%-%-----%---%-%-%-%-%"
    , "%-%-%-%%%%%%%-%-%%%%%%%%%-%%%-%-%%%-%"
    , "%-%-%-%-----%---%-----%-----%---%---%"
    , "%%%-%%%-%-%%%%%-%%%%%-%%%-%%%-%%%%%-%"
    , "%-----%-%-%-----%-%-----%-%---%-%-%-%"
    , "%-%-%-%-%-%%%-%%%-%%%-%%%-%-%-%-%-%-%"
    , "%-%-%-%-%-----------------%-%-%-----%"
    , "%%%-%%%%%%%-%-%-%%%%%-%%%-%-%%%-%%%%%"
    , "%-------%-%-%-%-----%---%-----%-%---%"
    , "%%%%%-%-%-%%%%%%%%%-%%%%%%%%%%%-%-%%%"
    , "%%%-%%%%%%%%%-%%%%%%%-%%%-%%%-%-%-%-%"
    , "%-------------%-------%-%---%-----%-%"
    , "%-%-%%%%%-%-%%%-%-%-%%%-%-%%%-%%%-%-%"
    , "%-%-%-----%-%-%-%-%-----%---%-%-%-%-%"
    , "%-%-%-%%%%%%%-%-%%%%%%%%%-%%%-%-%%%-%"
    , "%-%-%-%-----%---%-----%-----%---%---%"
    , "%%%-%%%-%-%%%%%-%%%%%-%%%-%%%-%%%%%-%"
    , "%-----%-%-%-----%-%-----%-%---%-%-%-%"
    , "%-%-%-%-%-%%%-%%%-%%%-%%%-%-%-%-%-%-%"
    , "%-%-%-%-%-----------------%-%-%-----%"
    , "%%%-%%%%%%%-%-%-%%%%%-%%%-%-%%%-%%%%%"
    , "%-------%-%-%-%-----%---%-----%-%---%"
    , "%%%%%-%-%-%%%%%%%%%-%%%%%%%%%%%-%-%%%"
    , "%%%-%%%%%%%%%-%%%%%%%-%%%-%%%-%-%-%-%"
    , "%-------------%-------%-%---%-----%-%"
    , "%-%-%%%%%-%-%%%-%-%-%%%-%-%%%-%%%-%-%"
    , "%-%-%-----%-%-%-%-%-----%---%-%-%-%-%"
    , "%-%-%-%%%%%%%-%-%%%%%%%%%-%%%-%-%%%-%"
    , "%-%-%-%-----%---%-----%-----%---%---%"
    , "%%%-%%%-%-%%%%%-%%%%%-%%%-%%%-%%%%%-%"
    , "%-----%-%-%-----%-%-----%-%---%-%-%-%"
    , "%-%-%-%-%-%%%-%%%-%%%-%%%-%-%-%-%-%-%"
    , "%-%-%-%-%-----------------%-%-%-----%"
    , "%%%-%%%%%%%-%-%-%%%%%-%%%-%-%%%-%%%%%"
    , "%-------%-%-%-%-----%---%-----%-%---%"
    , "%%%%%-%-%-%%%%%%%%%-%%%%%%%%%%%-%-%%%"
    , "%%%-%%%%%%%%%-%%%%%%%-%%%-%%%-%-%-%-%"
    , "%-------------%-------%-%---%-----%-%"
    , "%-%-%%%%%-%-%%%-%-%-%%%-%-%%%-%%%-%-%"
    , "%-%-%-----%-%-%-%-%-----%---%-%-%-%-%"
    , "%-%-%-%%%%%%%-%-%%%%%%%%%-%%%-%-%%%-%"
    , "%-%-%-%-----%---%-----%-----%---%---%"
    , "%%%-%%%-%-%%%%%-%%%%%-%%%-%%%-%%%%%-%"
    , "%-----%-%-%-----%-%-----%-%---%-%-%-%"
    , "%-%-%-%-%-%%%-%%%-%%%-%%%-%-%-%-%-%-%"
    , "%-%-%-%-%-----------------%-%-%-----%"
    , "%%%-%%%%%%%-%-%-%%%%%-%%%-%-%%%-%%%%%"
    , "%-------%-%-%-%-----%---%-----%-%---%"
    , "%%%%%-%-%-%%%%%%%%%-%%%%%%%%%%%-%-%%%"
    , "%%%-%%%%%%%%%-%%%%%%%-%%%-%%%-%-%-%-%"
    , "%-------------%-------%-%---%-----%-%"
    , "%-%-%%%%%-%-%%%-%-%-%%%-%-%%%-%%%-%-%"
    , "%-%-%-----%-%-%-%-%-----%---%-%-%-%-%"
    , "%-%-%-%%%%%%%-%-%%%%%%%%%-%%%-%-%%%-%"
    , "%-%-%-%-----%---%-----%-----%---%---%"
    , "%%%-%%%-%-%%%%%-%%%%%-%%%-%%%-%%%%%-%"
    , "%-----%-%-%-----%-%-----%-%---%-%-%-%"
    , "%-%-%-%-%-%%%-%%%-%%%-%%%-%-%-%-%-%-%"
    , "%-%-%-%-%-----------------%-%-%-----%"
    , "%%%-%%%%%%%-%-%-%%%%%-%%%-%-%%%-%%%%%"
    , "%-------%-%-%-%-----%---%-----%-%---%"
    , "%%%%%-%-%-%%%%%%%%%-%-%%%%%%%%%-%-%%%"    
    , "%---%-%-----------%-------%---%-%---%"
    , "%-%%%-%%%%%-%%%%%%%%%-%-%%%-%-%-%%%-%"
    , "%-%---%------%--------%-----%-------%"
    , "%-%-%-%%%%%-%%%-%-%-%-%-%%%%%%-%%%%%%"
    , "%-%-%---%-----%-%-%-%-------%---%-%-%"
    , "%-%-%%%-%%%-%-%-%-%%%%%%%%%-%%%-%-%-%"
    , "%-%---%-%---%-%-%-.-%-%---%-%-%-----%"
    , "%-%%%-%%%-%%%%%-%%%-%-%-%%%%%-%-%%%%%"
    , "%-------%---%-----%-%-----%---%-%---%"
    , "%%%-%-%%%%%-%%%%%-%%%-%%%-%-%%%-%-%%%"
    , "%-%-%-%-%-%-%-%-----%-%---%-%---%-%-%"
    , "%-%-%%%-%-%-%-%-%%%%%%%%%-%-%-%-%-%-%"
    , "%---%---%---%-----------------%-----%"
    , "%-%-%-%-%%%-%%%-%%%%%%%-%%%-%%%-%%%-%"
    , "%-%-%-%-------%---%-------%---%-%---%"
    , "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"]

