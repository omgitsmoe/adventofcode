import System.IO
import Util

main = do
    contents <- readFile "d07.in"
    let crabs = map (read::String->Int) . splitOn "," . trim $ contents
        minX = minimum crabs
        maxX = maximum crabs
        -- for all positions between min and max compute all the distances from that
        -- position and save it as tuple (sum of distances, position)
        dists = map (\p -> (sum . map (\x -> abs $ p - x) $ crabs, p)) [minX..maxX]
        -- minimum with tuples decides based on first element and then 2nd should the
        -- first be equal, so we put the distance sum first
        -- even though we don't really need the position only the distances
        bestPos = minimum dists
        -- fuel for dist is same as a sum of counting up the numbers to that dist
        -- which can be simpliefied like so:
        -- \sum_{i=1}^{n} i = 1 + 2 + 3 ... + n = (n*(n+1))/2
        fuelForDist d = (d * (d + 1)) `div` 2
        bestPos2 = minimum
            $ map (\p -> (sum . map (\x -> fuelForDist . abs $ p - x) $ crabs, p)) [minX..maxX]
    putStrLn $ "Part1: " ++ (show $ fst bestPos)
    putStrLn $ "Part2: " ++ (show $ fst bestPos2)

