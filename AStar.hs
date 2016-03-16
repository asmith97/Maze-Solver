import System.IO
import Data.List

data Tree a = Root (a,a) | Node (Tree a) (a,a) Int Int deriving Show
type Board = [String]
type Position = (Int, Int)

board = ["%%%%%%%%%%%%%%%%%%%%", "%--------------%---%"
            , "%-%%-%%-%%-%%-%%-%-%", "%--------P-------%-%", "%%%%%%%%%%%%%%%%%%-%"
            , "%.-----------------%", "%%%%%%%%%%%%%%%%%%%%"]

board' = ["%%%%%%%%%%%%%%%%%%%%", "%-------------.%---%"
            , "%%%%%%%%%%%%%%%%%%%%"]

board'' = ["%%%%", "%--.%"
            , "%%%%"]

getListOfVisited :: Tree Int -> [(Int, Int)]
getListOfVisited (Root a) = [a]
getListOfVisited (Node parent a _ _) = a : getListOfVisited parent

getG (Root _) = 0
getG (Node _ _ g _) = g

getF (Root _) = 0
getF (Node _ _ _ f) = f

getParent (Node parent _ _ _) = parent
getParent a@(Root _) = a

--WRITE FREE FOOD THING THAT PARSES YOUR EMAILS

getValidNeighbors :: Position -> Board -> [Position]
--valid when x, y > 0 and less than the board dimensions (in the middle)
getValidNeighbors (x,y) board = [(a,b) | (a,b) <- [(x, y+1), (x, y-1), (x+1, y), (x-1, y)], ((board !! b) !! a) /= '%']

isTarget :: Position -> Board -> Bool
isTarget (x,y) board = ((board !! y) !! x) == '.'

--Cost of moving to a given position given the position of the goal
--we will store the distances as the distance squared
calculateCost :: Tree Int -> Position -> Int
calculateCost (Node parent (x,y) g h) (goalX, goalY) = g + (goalY - y)^2 + (goalX - x) ^2
calculateCost (Root (x,y)) (goalX, goalY) = (goalY - y)^2 + (goalX - x) ^2

calcCost (x,y) (goalX, goalY) = (goalY - y)^2 + (goalX - x) ^2

--The tree int is the place we are starting from
--search :: Tree Int -> Board -> Position -> Tree Int
--search (Root (x,y)) board position = calculateCost (getValidNeighbors (x,y) board)


--want to make sure it doesn't go back on itself
validTrees :: Tree Int -> Board -> Position -> [Tree Int]
validTrees t@(Root (x,y)) board goal = (fmap (\a -> Node (Root (x,y)) a (calcCost (x,y) a) (calcCost a goal) )) $ filter (\x -> notElem x (getListOfVisited t)) $ getValidNeighbors (x,y) board
--validTrees t@(Root (x,y)) board goal = (fmap (\a -> Node (Root (x,y)) a (calcCost (x,y) a) (calcCost a goal) )) $ getValidNeighbors (x,y) board

{-validTrees (Node parent (x,y) _ _) board goal = fmap (func goal) $ getValidNeighbors (x,y) board
    where func goal pos = Node parent pos (getG parent + getF parent) (calcCost pos goal) -}
validTrees new@(Node parent (x,y) g h) board goal =  (fmap (\a -> Node new a (g + (calcCost (x,y) a)) (calcCost a goal))) $ filter (\x -> notElem x (getListOfVisited new)) $ getValidNeighbors (x,y) board
--validTrees new@(Node parent (x,y) g h) board goal =  (fmap (\a -> Node new a (g + (calcCost (x,y) a)) (calcCost a goal))) $ getValidNeighbors (x,y) board

    --where func goal pos = Node new pos (g+h) (calcCost pos goal)
    --where f = fmap (\a -> Node new a (g+h) (calcCost a goal))

--need to factor in when we reach the goal
--search :: Tree Int -> Board -> Position
--search tree board goal = fmap (\a -> search a board goal) 


r = Root (1,1) :: Tree Int
g = (2,5) :: Position

printBoard = mapM_ putStrLn board

sortTreeList :: [Tree Int] -> [Tree Int]
sortTreeList [] = []
sortTreeList (a:as) = (sortTreeList lower) ++ [a] ++ (sortTreeList upper) where
    lower = [x | x <- as, (getG x + getF x) < (getG a + getF a)]
    upper = [x | x <- as, (getG x + getF x) > (getG a + getF a)]


{-
Search process:
At a point in the map START:
Find the direction which has the smallest associated cost
Add this direction to the list of places we've gone
Don't double cross for as long as we're going down this branch
If there are no more valid moves, then end this branch, return back to the 
START, and then pick the one with the second smallest cost
-}

--for now just keep on returning the first element of the list
--later, do something like:
-- if (head sortTreeList...) = something to do with finding goal
--then return head, else go to the next one
--not that big of an issue because there's only ever a max of 4 possible directions
--seems to work in the case where the heuristic is always guiding along the right path

--need to get it to be able to back track
--the condition for it to back track is when:
--want to avoid the path doubling back on itself
{-}
search :: Tree Int -> Board -> Position -> [Position] -> Tree Int
search start@(Root (x,y)) board goal [] = if (x,y) == goal then start else search (head $ sortTreeList (validTrees start board goal)) board goal []
--if it's an empty list, want to go to the second best one of the parent (if that exists)
--if that doesn't then keep going up
--could implement it with another variable argument that's a counter of the number to skip
search start@(Node parent (x,y) g h) board goal [] = if (x,y) == goal then start else search (head $ sortTreeList (validTrees start board goal)) board goal []
--this should never happen
search (Root (x,y)) board goal (a:as) = undefined
search (Node parent (x,y) g h) board goal visited = undefined-}

search start@(Root (x,y)) board goal skip parentSkip [] = if (x,y) == goal then start
    else
        search (sortedList !! skip) board goal skip parentSkip []
    where
        sortedList = sortTreeList $ validTrees start board goal

search start@(Node parent (x,y) g h) board goal skip parentSkip [] = if (x,y) == goal then start 
    else
        if length sortedList > skip then search (sortedList !! skip) board goal skip parentSkip []
            else
                if length sortedList > skip + 1 then 
                    search start board goal (skip + 1) parentSkip []
                else
                    search parent board goal (parentSkip +1) 0 []
                --if after adding one to skip its still less than the length, then
                --increment skip and do the search on the same one
                --if it is equal to skip then want to do the search on the 
                --parent after incrementing the skip by one
    where
        sortedList = sortTreeList $ validTrees start board goal


 