import System.IO
import Data.List(foldl')
import qualified Data.Map.Strict as Map
import Debug.Trace

-- haskell also doesn't have split on substring function................
splitOn' :: [String] -> Int -> String -> String -> [String]
-- since we used consing/prepend to build the accumulator for our split strings
-- reverse the individual strings as well the "split elements" order in the list
splitOn' acc matchidx s [] = reverse . map reverse
    $ if (head acc) /= []
        then acc
        -- drop empty element from acc
        else drop 1 acc
splitOn' acc matchidx s (x:xs)
    | x == (s !! matchidx) =
        if matchidx == (splitStrLen - 1)
            then splitOn' onMatchAcc 0 s xs
            else splitOn' charAddedAcc (matchidx + 1) s xs
    | otherwise = splitOn' charAddedAcc 0 s xs
    -- NOTE: bindings here would only be executed if there are used somewhere in control flow for
    -- the current case (due to lazy evaluation) so we can have all the bindings in the where
    -- clause without worrying about them being computed for every time without needing them
    where
        -- add current char to current first string in accumulator
        charAddedAcc = (x:(head acc)):(drop 1 acc)
        splitStrLen = length s
        -- on match remove the splitStrLen we accumulated so far (missing 1/current char)
        -- in case we didn't complete the match
        currentSplit = drop (splitStrLen - 1) $ head acc 
        -- don't add empty elements
        onMatchAcc = if currentSplit /= []
            -- we also need to prepend a new empty element for accumulating the next
            -- substring
            then "":currentSplit:(drop 1 acc)
            else "":(drop 1 acc)

-- so we can pre-curry the function to take the initial state, so the user doesn't have
-- look it up or care about it at all
splitOn = splitOn' [""] 0

-- haskell aaalso doesn't have a split on char function............
-- (i know i could've reused my splitOn)
-- which the below is doing other than not taking in a param for the delim char
-- and converting the split items to int
coordFromStr [] = error "Can't convert coordinate from an empty string!"
coordFromStr s = (xPart, yPart)
    where
        -- quick helper function to retrieve an element's idx in a list
        getElemIdx i e [] = -1
        getElemIdx i e (x:xs)
            | e == x = i
            | otherwise = getElemIdx (i + 1) e xs
        delimIdx = getElemIdx 0 ',' s
        -- delimIdx is our boundary which marks the end of our xpart
        xPart = (read::String->Int) $ take delimIdx s
        yPart = (read::String->Int) $ drop (delimIdx + 1) s

-- accumulator function that takes a point pair and generates all the point in the
-- vertical or horizontal line that the pair forms
-- the points will be part of a tuple ((x, y), 1) where the 1 will later be used
-- for summing up overlapping points when using fromListWith
accCoordsNonDiagonal acc [(x1, y1), (x2, y2)]
    -- discard diagonals for part1
    | x1 /= x2 && y1 /= y2 = acc
    -- zip x and y ranges together then zip the result with infinite list of 1, 1, ..
    -- and prepend it to our accumulator
    | otherwise = (zip (zip xs ys) (repeat 1)):acc
    where
        -- row == keep y fixed <-> col == keep x fixed
        isRowVent = y1 == y2
        xs = if isRowVent then [(min x1 x2)..(max x1 x2)] else repeat x1
        ys = if isRowVent then repeat y1 else [(min y1 y2)..(max y1 y2)]

main = do
    contents <- readFile "d05.in"
    -- convert to lines, split the vent lines and save the 2 coordinate tuples
    -- haskell doesn't even have a trim function :/
    let vents = map (map coordFromStr . splitOn " -> ") . filter (/= []) . lines $ contents
        coordVentAmounts =
            -- convert the list to a map
            -- same as fromList which creates a Map from an associative list [(k, v), ..]
            -- but keys are not overwritten instead we pass it a function that is applied
            -- on conflicts:
            -- we just pass + so the amounts get added
            Map.fromListWith (+)
            -- build associative list of style [((x, y), 1), ..] with all the coordinates where
            -- a went appears so we can built a map from it
            -- the 1 represents the amount of vent lines in this case, will be summed up later
            -- using fromListWith
            -- need to flatten/concat as last step since the ranges were still in a list per line
            -- ((concat $ foldl' (\acc [(x1, y1), (x2, y2)] ->
            --     -- discard diagonals for part1
            --     if x1 /= x2 && y1 /= y2 then acc else
            --     let
            --         -- row == keep y fixed <-> col == keep x fixed
            --         isRowVent = y1 == y2;
            --         xs = if isRowVent then [(min x1 x2)..(max x1 x2)] else repeat x1
            --         ys = if isRowVent then repeat y1 else [(min y1 y2)..(max y1 y2)]
            --     in
            --         -- zip x and y ranges together then zip the result with infinite list of 1, 1, ..
            --         -- and prepend it to our accumulator
            --         (zip (zip xs ys) (repeat 1)):acc) [] vents)::[((Int, Int), Int)])
            ((concat $ foldl' accCoordsNonDiagonal [] vents)::[((Int, Int), Int)])
        coordVentAmounts2 =
            Map.fromListWith (+)
            ((concat $ foldl' (\acc [(x1, y1), (x2, y2)] ->
                -- diff behaviour for diagonals
                if x1 /= x2 && y1 /= y2
                    then
                        let
                            xs' = [(min x1 x2)..(max x1 x2)]
                            -- need to respect the direction of diagonals:
                            -- check if we need to reverse the generated range otherwise
                            -- zipped pairs won't match the actual coordinates in the 45deg vent line
                            xs = if x1 > x2 then reverse xs' else xs'
                            ys' = [(min y1 y2)..(max y1 y2)]
                            ys = if y1 > y2 then reverse ys' else ys'
                        in
                            (zip (zip xs ys) (repeat 1)):acc
                    else
                        accCoordsNonDiagonal acc [(x1, y1), (x2, y2)]
                    ) [] vents)::[((Int, Int), Int)])
        -- Map.foldl only has the value as param
        getOverlapping = Map.foldl (\acc n -> if n > 1 then acc + 1 else acc) 0

    putStrLn ("Part1: " ++ (show . getOverlapping) coordVentAmounts)
    putStrLn ("Part2: " ++ (show . getOverlapping) coordVentAmounts2)
