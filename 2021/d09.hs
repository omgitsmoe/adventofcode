import System.IO
import Data.Char(digitToInt)
import Data.Maybe(fromJust)
import Data.Array(Array, array, (!), inRange, bounds)
import Data.List(sortBy)
import qualified Data.Set as Set

import Util

type Point = (Int, Int)

-- using arr part1: ~40ms
-- using lists and !! (d09_b4_arr.hs): ~60ms
main = do
    contents <- readFile "d09.in"
    -- read converts a STRING to a readable element
    -- so either I use pure to wrap it inside an applicative value
    -- map (read . pure :: Char->Int) "123"
    -- or use digitToInt
    -- very unhelpful type mismatch error message btw, since I specified
    -- read as read::Char->Int which is illegal, but that was not the error
    let input = map (\l -> map digitToInt l) . lines . trim $ contents
        caveDimX = length $ head input
        caveDimY = length input
        -- array needs the min/max bounds as __indices__
        -- zip [0..] ls is basically equivalent to enumerate(ls) in python
        -- we also enumerate all the rows so we get x coords
        locations =
            array ((0, 0), (caveDimX - 1, caveDimY - 1))
                -- y will be the zipped index 0.., row will hold the result of a row in the input and
                -- zip [0..], pattern matching on row will mean we have a value for every y and x
                -- combination since there will be one permutation for every <- in the list comprehension
                [((x, y), v) | (y, row) <- zip [0..] . map (\row -> zip [0..] row) $ input, (x, v) <- row]
        -- NOTE: still kind of imperative style mb? but at least we're using the proper data structure
        -- most answers online recommened to use an Array for grids, so this actually seems to
        -- be the way to do it idiomatically (checked after doing it)
        -- Array has the '!' operator to access arr at a specific index
        getNeighbours :: Point -> Array (Int, Int) Int -> [Maybe Int]
        getNeighbours (x, y) arr = [
                if inRange (bounds arr) above then Just (arr ! above) else Nothing,
                if inRange (bounds arr) right then Just (arr ! right) else Nothing,
                if inRange (bounds arr) below then Just (arr ! below) else Nothing,
                if inRange (bounds arr) left  then Just (arr ! left)  else Nothing
            ]
            where 
                above = (x, y - 1)
                right = (x + 1, y)
                below = (x, y + 1)
                left  = (x - 1, y)
        -- Nothing will always be less than any Just value, we want the opposite
        minmb (Just x) (Just y) = if x < y then Just x else Just y
        minmb (Just x) Nothing = Just x
        minmb Nothing (Just y) = Just y
        minmb Nothing Nothing = Nothing
        -- find minimum of list with maybes using minmb
        minimummb (x:[]) = x
        minimummb (x:y:xs) = minimummb ((minmb x y):xs)
        -- could use the where here to use a helper function so that a user doesn't have to
        -- pass initial state (seen this a lot in stackoverflow answers)
        -- only works for a single pattern though otherwise the helper would not be visible to the others
        getLowPoints :: (Int, Int) -> Array (Int, Int) Int -> [Point]
        getLowPoints p ps = go [] p ps
            where
                -- having this on the same line as the where would always result in a parse error?!?!?
                -- but for the where below it's fine??!?!
                go lowPoints (x, y) ps
                    | inRange (bounds ps) (x, y) = if locHeight < (minimummb $ getNeighbours (x, y) ps)
                        then go ((x, y):lowPoints) (nx, ny) ps
                        else go lowPoints (nx, ny) ps
                    | otherwise = lowPoints
                    -- y + 1 might be out of bounds but will be checked on next call
                    where (nx, ny) = if inRange (bounds ps) (x + 1, y) then (x + 1, y) else (0, y + 1)
                          locHeight = Just (ps ! (x, y))
        lowPointIndices = getLowPoints (0, 0) $ locations
        -- get the size of one basin by exploring outwards in all cardinal directions eliminating
        -- multiple visits by tracking visited locations
        getBasinSize :: Int -> Set.Set (Int, Int) -> Array (Int, Int) Int -> [(Int, Int)] -> Int
        getBasinSize size visited larr [] = size
        -- take first node/cell to visit from tovisit list
        -- we need the tovisit list since using multiple recursive calls would result in
        -- visited states being out of sync
        getBasinSize size visited larr ((x, y):ps)
            -- if in range explore the basin in all cardinal directions
            -- if current position < 9 count as part of the basin
            -- otherwise stop exploring
            | inRange (bounds larr) (x, y) && not (Set.member (x, y) visited) =
                if (larr ! (x, y)) < 9
                then getBasinSize (size + 1) newVisited larr newToVisit
                else getBasinSize size newVisited larr ps
            -- not in range stop exploring the basin
            | otherwise = getBasinSize size newVisited larr ps
            where
                -- NOTE: tried using recursive calls for each separate cardinal direction
                -- but this won't work since the visited set will become out of sync and nodes
                -- will be visited twice, so we instead have to implement like a bfs where we
                -- have a queue of nodes to visit and just recurce "once"
                -- add current cell to visited
                newVisited = Set.insert (x, y) visited
                above = (x, y - 1)
                right = (x + 1, y)
                below = (x, y + 1)
                left  = (x - 1, y)
                -- NOTE: this will mean we visit nodes up to 4 times (if there were 4 neighbours)
                -- we should do a conditional prepend, but no matter of searching found a solution for
                -- it (I've seen some conditional appends with monads but for this we could just use
                -- the ++ manually, but we want to avoid ++ since the operation will be O(n) for n
                -- elements in the lhs list)
                -- newToVisit = above:right:below:left:ps
                -- this is the naive and ugly solution for doing a conditional prepend
                -- very error prone! just using the wrong prev value bonks it
                -- nv1 = if not (Set.member above visited) then above:ps else ps
                -- nv2 = if not (Set.member right visited) then right:nv1 else nv1
                -- nv3 = if not (Set.member below visited) then below:nv2 else nv2
                -- nv4 = if not (Set.member left  visited) then left:nv3  else nv3
                -- newToVisit = nv4
                -- AHHH we could just use the new list with added (max 4) elements as lhs
                -- so we know the ++ will be cheap (why did I not think of that?, prob since
                -- that operation would be more costly in imperative languages)
                -- since ++ are just recursive prepends iterating over the lhs:
                -- (++) (a x:xs) ys = x : xs ++ ys
                -- here we flatten the list with concat, where empty lists (if condition after |
                -- not met) will be discarded
                -- newToVisit = concat [
                --         -- will be empty list if condition of above not being in visited is false
                --         -- will be empty if above was already visited
                --         [ above | not (Set.member above visited) ],
                --         [ right | not (Set.member right visited) ],
                --         [ below | not (Set.member below visited) ],
                --         [ left  | not (Set.member left  visited) ] ]
                --         ++ ps
                -- version without concat, but concat is just `foldr (++) []` internally
                -- so this and the above are prob equivalent
                -- due to ++ just being recursive prepends over the lhs, this and the version with concat
                -- are efficient versions of doing conditional prepends
                -- these are list comprehensions basically with |
                newToVisit =
                    -- will be empty list if condition of above not being in visited is false
                    -- = will be empty if above was already visited
                    [ above | not (Set.member above visited) ]
                    ++ [ right | not (Set.member right visited) ]
                    ++ [ below | not (Set.member below visited) ]
                    ++ [ left  | not (Set.member left  visited) ]
                    ++ ps
        -- flip arguments to compare to get descending sort
        sortDesc = sortBy (flip compare)
        -- sorted since we only need the 3 largest ones
        basinSizes = sortDesc . map (\x -> getBasinSize 0 Set.empty locations [x]) $ lowPointIndices

    putStrLn ("Part1: " ++ (show
        -- get low points as indices, extract them add one to each and sum it up
        $ sum . map (+ 1) . map (locations !) $ lowPointIndices))
    -- could use 'product' to multiply all list items together or the equivalent version doing it
    -- manually with foldr where 1 is the starting value for the accumulator
    putStrLn ("Part2: " ++ (show $ foldr (*) 1 $ take 3 basinSizes))
