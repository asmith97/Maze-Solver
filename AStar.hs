import System.IO
import System.Environment

data Tree a = Root (a,a) | Node (Tree a) (a,a) Int Int deriving (Show, Eq)
type Board = [String]
type Position = (Int, Int)

--this has a couple of bugs
--for example, it can successfully solve bigboard when the goal is (3,35)
--but not when the goal is (1,35) it makes a few successful backtracks while going to
--(3,35) but once it gets to (3,35) it jumps back to (2,29) which is too far for it to jump back to,
--so then it cannot solve it.  With slight modifications it should be able to not jump back too far

main :: IO ()
main = do
    args <- getArgs
    board <- readFile (args !! 0)
    let boardArray = lines board
    putStr "Input the starting coordinates (x,y): "
    hFlush stdout
    startCoord <- getLine
    putStr "Input the goal (x,y): "
    hFlush stdout
    endCoord <- getLine
    let posTuple = (read startCoord :: (Int, Int))
    let validPos = check posTuple boardArray
    let endTuple = (read endCoord :: (Int, Int))
    let validEndPos = check endTuple boardArray
    if and $ fmap not [validPos, validEndPos] then
            putStrLn "The inputted coordinates are not valid on the boards or are in a wall"
    else
        do
            let path = findPath (Root posTuple) boardArray endTuple
            mapM_ putStrLn boardArray
            putStrLn $ printSolution (toArray path)

toArray :: Tree Int -> [Position]
toArray (Root (0,0)) = []
toArray (Root t) = [t]
toArray (Node parent tuple _ _) = (toArray parent) ++ [tuple]

printSolution :: [Position] -> String
printSolution [] = "No solution given this initial value"
printSolution a = "Has length " ++ (show $ length a) ++ "\n" ++ show a

check :: Position -> Board -> Bool
check _ [] = True
check (x,y) board
    | x < xMax && y < yMax && ((board !! y) !! x) /= '%' = True
    | otherwise = False
        where
            yMax = length board
            xMax = length $ board !! 0


getListOfVisited :: Tree Int -> [(Int, Int)]
getListOfVisited (Root a) = [a]
getListOfVisited (Node parent a _ _) = a : getListOfVisited parent

getG (Root _) = 0
getG (Node _ _ g _) = g

getF (Root _) = 0
getF (Node _ _ _ f) = f

getValidNeighbors :: Position -> Board -> [Position]
--valid when x, y > 0 and less than the board dimensions (in the middle)
getValidNeighbors (x,y) board = [(a,b) | (a,b) <- [(x, y+1), (x, y-1), (x+1, y), (x-1, y)], ((board !! b) !! a) /= '%']


--Cost of moving to a given position given the position of the goal
--we will store the distances as the distance squared
calcCost :: Position -> Position -> Int
calcCost (x,y) (goalX, goalY) = (goalY - y)^2 + (goalX - x) ^2


--want to make sure it doesn't go back on itself
validTrees :: Tree Int -> Board -> Position -> [Tree Int]
validTrees t@(Root (x,y)) board goal = (fmap (\a -> Node (Root (x,y)) a (calcCost (x,y) a) (calcCost a goal))) $ filter (\e -> notElem e (getListOfVisited t)) $ getValidNeighbors (x,y) board
validTrees new@(Node parent (x,y) g h) board goal =  (fmap (\a -> Node new a (g + (calcCost (x,y) a)) (calcCost a goal))) $ filter (\x -> notElem x (getListOfVisited new)) $ getValidNeighbors (x,y) board


sortTreeList :: [Tree Int] -> [Tree Int]
sortTreeList [] = []
sortTreeList (a:as) = (sortTreeList lower) ++ [a] ++ (sortTreeList upper) where
    lower = [x | x <- as, (getG x + getF x) < (getG a + getF a)]
    upper = [x | x <- as, (getG x + getF x) > (getG a + getF a)]


branch :: [Tree Int] -> Board -> Position -> Tree Int
branch [] board goal = Root (0,0)
branch (a:as) board goal
    | result == Root (0,0)= branch as board goal
    | otherwise = result
        where
            result = findPath a board goal

findPath :: Tree Int -> Board -> Position -> Tree Int
findPath start@(Root (x,y)) board goal = if (x,y) == goal then start
    else
        --return this in the case where there is no solution
        if null sorted then (Root (0,0))
        else
            branch sorted board goal
    where 
        sorted = sortTreeList $ validTrees start board goal

findPath node@(Node parent (x,y) g h) board goal = if (x,y) == goal then node
    else
        if null sorted then (Root (0,0))
        else
            branch sorted board goal
    where
        sorted = sortTreeList $ validTrees node board goal


{-
Search process:
At a point in the map START:
find the valid neighbors
sort in order of cost
go down the one with the smallest cost
    if it leads to the end, then done
if it does not, then back track to the one with the next smallest cost
    and keep repeating this until either there are no more valid paths (in which case it cannot be solved)
    or it gets to the goal, in which case we are done
-}

bigboard = ["%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
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
    , "%-%.%-%-------%---%-------%---%-%---%"
    , "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"]
 