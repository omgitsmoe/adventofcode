import System.IO
import Data.List(foldl', (\\), sort)
import Data.Char(digitToInt)
import Data.Maybe(fromJust)
import qualified Data.Map.Strict as Map

import Util
import Debug.Trace

type Point = (Int, Int)

main = do
    contents <- readFile "d09.in"
    -- read converts a STRING to a readable element
    -- so either I use pure to wrap it inside an applicative value
    -- map (read . pure :: Char->Int) "123"
    -- or use digitToInt
    -- very unhelpful type mismatch error message btw, since I specified
    -- read as read::Char->Int which is illegal, but that was not the error
    let locations = map (\l -> map digitToInt l) . lines . trim $ contents
        caveDimX = length $ head locations
        caveDimY = length locations
        inBounds (x, y) = (0 <= x) && (x < caveDimX) && (0 <= y) && (y < caveDimY)
        -- NOTE: imperative style since we use !! to get the cells -> prob inefficient
        -- since the list to that point has to be traversed
        -- TODO mb use a map instead?, but would be slow compared to using a 2d array
        -- in an imperative language
        getNeighbours :: Point -> [[Int]] -> [Maybe Int]
        getNeighbours (x, y) ps = [
                if inBounds (x, y - 1) then Just (rowAbove !! x)  else Nothing,
                if inBounds (x + 1, y) then Just (row !! (x + 1)) else Nothing,
                if inBounds (x, y + 1) then Just (rowBelow !! x)  else Nothing,
                if inBounds (x - 1, y) then Just (row !! (x - 1)) else Nothing
            ]
            where rowAbove = ps !! (y - 1)
                  row = ps !! y
                  rowBelow = ps !! (y + 1)
        -- Nothing will always be less than any Just value, we want the opposite
        minmb (Just x) (Just y) = if x < y then Just x else Just y
        minmb (Just x) Nothing = Just x
        minmb Nothing (Just y) = Just y
        minmb Nothing Nothing = Nothing
        -- find minimum of list with maybes using minmb
        minimummb (x:[]) = x
        minimummb (x:y:xs) = minimummb ((minmb x y):xs)
        getLowPoints :: [Point] -> (Int, Int) -> [[Int]] -> [Point]
        getLowPoints lowPoints (x, y) ps
            | inBounds (x, y) = if locHeight < (minimummb $ getNeighbours (x, y) ps)
                then getLowPoints ((x, y):lowPoints) (nx, ny) ps
                else getLowPoints lowPoints (nx, ny) ps
            | otherwise = lowPoints
            -- y + 1 might be out of bounds but will be checked on next call
            where (nx, ny) = if inBounds (x + 1, y) then (x + 1, y) else (0, y + 1)
                  locHeight = Just ((ps !! y) !! x)
        -- very imperative style: get all pts in ls from a list of indices ps
        getIndices heights ls [] = heights
        getIndices heights ls (p:ps) = getIndices ((ls !! (snd p)) !! (fst p):heights) ls ps

    putStrLn ("Part1: " ++ (show
        -- get low points as indices, extract them add one to each and sum it up
        $ sum . map (+ 1) . getIndices [] locations . getLowPoints [] (0, 0) $ locations))
