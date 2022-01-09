import System.IO
import Data.Char(digitToInt)
import Data.Array(Array, (//), (!), inRange, bounds, listArray, assocs, elems)
import Data.List(foldl')

import Util


main = do
    contents <- readFile "d11.in"
    let input = map (\l -> map digitToInt l) . lines . trim $ contents
        gridSize = length input
        -- listArray expects a flat list which is why we use concat
        grid = listArray ((0, 0), (9, 9)) . concat $ input
        oneStep :: Array (Int, Int) Int -> (Array (Int, Int) Int, Int)
        -- reset flashed octs back to 0
        oneStep g = (stepped // [(i, if e > 9 then 0 else e) | (i, e) <- assocs stepped],
                     flashes)
            where
                -- inc all octs by 1
                -- (g // l) operator changes array g by association list of [(idx, element), ..]
                plusOne = g // [(i, e + 1) | (i, e) <- assocs g]
                -- only flash on 10 since octs only can flash once
                flashedAfterFirstInc = [i | (i, e) <- assocs plusOne, e == 10]
                -- compute one step by starting with octs that started flashing after the first inc
                -- we keep track of neihgbouring octs that also started flashing as a list
                -- and flash them recursively
                (stepped, flashes) = flash flashedAfterFirstInc plusOne (length flashedAfterFirstInc)
                flash :: [(Int, Int)] -> Array (Int, Int) Int -> Int -> (Array (Int, Int) Int, Int)
                flash [] g flashes = (g, flashes)
                -- flash octs at indices (x,y):ps
                flash ((x, y):ps) g flashes = flash newPs (g // assocNew) (flashes + (length newFlashes))
                    where
                        gb = bounds g
                        -- ALL (including diagonals) adjacent octs get incremented by 1 after an
                        -- oct flashes
                        -- check if neighbours are in the grid using inRange
                        neighbours = [(nx, ny) | nx <- [x - 1, x, x + 1], ny <- [y - 1, y, y + 1],
                                      (nx, ny) /= (x, y) && inRange gb (nx, ny)]
                        -- (idx in grid, new value which was incremented by 1)
                        assocNew = zip neighbours [(g ! i) + 1 | i <- neighbours]
                        newFlashes = [i | (i, e) <- assocNew, e == 10]
                        newPs = newFlashes ++ ps
        -- repeat the oneStep function 100 times using foldl's accumulator to keep track
        -- of the number of flashes and to pass along the new grid
        flashesAfter100Steps =
            snd $ foldl' (\(g, n) _ ->
                let (newGrid, flashes) = oneStep g in (newGrid, n + flashes))
            (grid, 0) [1..100]
        -- check after each step if all octs have flashed which would result in a
        -- array sum of 0 and return the number of steps we took to reach that state
        stepsForSync g = go 1 g
            where go s g =
                    if (sum $ elems newGrid) == 0
                    then s
                    else go (s + 1) newGrid
                        where
                            (newGrid, flashes) = oneStep g

    putStrLn $ "Part1: " ++ (show flashesAfter100Steps)
    putStrLn $ "Part2: " ++ (show $ stepsForSync grid)
