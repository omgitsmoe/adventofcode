import System.IO
import Data.Char(digitToInt)
import Data.Maybe(fromJust)
import Data.Array(Array, array, (!), inRange, bounds)
import qualified Data.Set as Set

import Util
import Debug.Trace

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
        getLowPoints :: [Point] -> (Int, Int) -> Array (Int, Int) Int -> [Point]
        getLowPoints lowPoints (x, y) ps
            | inRange (bounds ps) (x, y) = if locHeight < (minimummb $ getNeighbours (x, y) ps)
                then getLowPoints ((x, y):lowPoints) (nx, ny) ps
                else getLowPoints lowPoints (nx, ny) ps
            | otherwise = lowPoints
            -- y + 1 might be out of bounds but will be checked on next call
            where (nx, ny) = if inRange (bounds ps) (x + 1, y) then (x + 1, y) else (0, y + 1)
                  locHeight = Just (ps ! (x, y))
        lowPointIndices = getLowPoints [] (0, 0) $ locations
        getBasinSize visited larr (x, y)
            -- if in range explore the basin in all cardinal directions
            -- if current position < 9 count as part of the basin
            -- otherwise stop exploring
            | inRange (bounds larr) (x, y) =
                if (larr ! (x, y)) < 9
                then 1 + neighbours
                else 0
            -- not in range stop exploring the basin
            | otherwise = 0
            where
                -- NOTE: tried using recursive calls for each separate cardinal direction
                -- but this won't work since the visited set will become out of sync and nodes
                -- will be visited twice, so we instead have to implement like a bfs where we
                -- have a queue of nodes to visit and just recurce "once"
                -- add neighbours to set
                -- as well as current point, which is technically only needed for the starting point
                -- but should not matter performance wise
                newVisited = Set.union visited $ Set.fromList [(x, y), above, right, below, left]
                -- do the visited check here instead of on the current point, since
                -- otherwise the different recursive calls will not have the other neighbours
                -- we visit here in the set
                above = (x, y - 1)
                right = (x + 1, y)
                below = (x, y + 1)
                left  = (x - 1, y)
                aboveNum = if not (Set.member above visited)
                    then getBasinSize newVisited larr above
                    else 0
                rightNum = if not (Set.member right visited)
                    then getBasinSize newVisited larr right
                    else 0
                belowNum = if not (Set.member below visited)
                    then getBasinSize newVisited larr below
                    else 0
                leftNum  = if not (Set.member left visited)
                    then getBasinSize newVisited larr left
                    else 0
                neighbours = aboveNum + rightNum + belowNum + leftNum

    -- putStrLn ("Part1: " ++ (show
    --     -- get low points as indices, extract them add one to each and sum it up
    --     $ sum . map (+ 1) . map (locations !) $ lowPointIndices))
    putStrLn $ show $ zip lowPointIndices (map (getBasinSize Set.empty locations) lowPointIndices)
