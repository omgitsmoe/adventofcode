import System.IO
import Data.List(span, intercalate)
import qualified Data.Set as Set
import qualified Data.Array as Array

import Data.Time

import Util

main = do
    contents <- readFile "d13.in"
    let input = lines . trim $ contents
        -- consume from input while we don't encounter an empty line
        -- split x,y and convert to int transforming the result into a bintuple
        coords =
            map ((\[x, y] -> (x, y)) . map (read::String->Int) . splitOn ",") . takeWhile (/= "") $ input
        dots = Set.fromList coords
        -- drop "fold along " and split in (axis, amount)
        folds = map (\l ->
            let [axis, amount] = splitOn "=" . drop 11 $ l in
                (axis, read amount :: Int))
            -- skip coords and empty line
            . drop 1 . dropWhile (/= "") $ input
        fold :: (String, Int) -> Set.Set (Int, Int) -> Set.Set (Int, Int)
        fold (axis, foldCoord) dots =
            Set.map (\dot -> case getCoord dot of
                v
                    -- point lies beyond fold line
                    -- new coordinate = foldLine - distance from fold line
                    | v > foldCoord -> rebuildPoint (foldCoord - (v - foldCoord)) dot
                    | otherwise -> dot) dots
            where
                -- helper functions so we can generalize the above for x/y
                (getCoord, rebuildPoint) = case axis of
                    "x" -> (fst, (\v p -> (v, snd p)))
                    "y" -> (snd, (\v p -> (fst p, v)))
                    _ -> error "Wrong axis!"
        doFolds [] dots = dots
        doFolds (f:fs) dots = doFolds fs (fold f dots)
        -- build assoc list of dot and marker
        -- and mark them in our grid
        renderDots dots = intercalate "\n"
                [[if Set.member (x, y) dots then '#' else '.' | x <- [minX..maxX]]
                 | y <- [minY..maxY]] where
            -- better/shorter to just map fst dots and then use minimum on the result
            min g = Set.foldr (\d acc -> let v = g d in if v < acc then v else acc) 10000
            max g = Set.foldr (\d acc -> let v = g d in if v > acc then v else acc) 0
            minX = min fst dots
            minY = min snd dots
            maxX = max fst dots
            maxY = max snd dots

    putStrLn $ "Part1: " ++ (show . Set.size . fold (head folds) $ dots)
    start <- getCurrentTime
    putStrLn $ "Part2:\n" ++ (renderDots . doFolds folds $ dots)
    stop <- getCurrentTime
    print $ diffUTCTime stop start
    -- F# ~15ms for part2
    -- Haskell: ~3ms (ghci or compiled same speed) for part2
