import System.IO
import Data.List(foldl')

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


main = do
    contents <- readFile "d06.in"
    let daysUntilNewFish = ((map (read::String->Int)) . splitOn ",") contents
        simOneFish x = if x == 0 then (6, Just 8) else (x - 1, Nothing)
        simOneDay [] = error "Can't sim without any lantern fish"
        simOneDay (x:xs) = case oneFish of
            (newx, Just newfish) -> newx:newfish:nextStep
            (newx, Nothing) -> newx:nextStep
            where
                oneFish = simOneFish x
                nextStep = if xs /= [] then simOneDay xs else []
        simOneFishXDays x d
            | x <= d = 1 + allChildren
            | otherwise = 1
            where
                regDaysLeft = d - x
                -- -- one additional offspring for the first irregular (< or > 7) interval
                -- directChildren = 1 + (regDaysLeft `mod` 7)
                -- create infinite list where n+1= n - 7; ..0 -> will end if it goes below 0
                -- e.g.: [100, 100-7..0] -> [100,93,86..2]
                allChildren =
                    sum $ map (\nd -> simOneFishXDays 9 nd) [regDaysLeft, regDaysLeft - 7..0]
        -- convert daysUntilNewFish to form that simOneStepByDay expects, namely
        -- (daysLeft, nrFishes)
        -- so we save the amount of fishes per days left
        nrFishPerDaysLeft =
            map (\x -> (x, countElem x daysUntilNewFish)) [0..8]
            where
                countElem e l = length $ filter (\x -> x == e) l
        -- only store amount of fishes per day
        simOneStepByDay :: [(Int, Int)] -> [(Int, Int)]
        simOneStepByDay state =
            foldl'
                (\acc (dayidx, nfishes) -> if dayidx == 0
                    -- decrement daysleft and spawn new fishes with 9 days left or dayidx 8 for
                    -- fishes with dayidx 0 and reset them to dayidx 6
                    -- NOTE: we look up the current day 7 to avoid having duplicate values for d6
                    then let (_, oldD7Fishes) = head $ filter (\(d, _) -> d == 7) state in
                        (8, nfishes):(6, nfishes + oldD7Fishes):acc
                    -- skip dayidx 7 since we already handled d7tod6 above
                    -- NOTE: this could turn into a situation where d7tod6 isn't handled because there
                    -- is no d0, only if there are d7 in the puzzle input
                    -- but how I understood the task description that can't be the case, since they
                    -- haven't started replicating yet and they only can have dayidx > 6 as a new fish
                    else if dayidx == 7 then acc else (dayidx - 1, nfishes):acc) [] state
                
    -- iterate: create infinite list of passing the result a of a function f to that
    -- same function: iterate f a --> [a, f(a), f(f(a)), ...]
    -- extracting element at idx 80 (81st element since 0th element is just initial day 0),
    -- which is day 80 of sim, evaluates the list to that point
    -- putStrLn ("Part1: " ++ (show $ length ((iterate simOneDay daysUntilNewFish) !! 80)))
    -- putStrLn ("Part2: " ++ (show $ (sum . map (\x -> simOneFishXDays (x + 1) 80)) daysUntilNewFish))
    putStrLn ("Part1: " ++ (show
        $ foldl' (\acc (d, x) -> acc + x) 0 ((iterate simOneStepByDay nrFishPerDaysLeft) !! 80)))
    putStrLn ("Part2: " ++ (show
        $ foldl' (\acc (d, x) -> acc + x) 0 ((iterate simOneStepByDay nrFishPerDaysLeft) !! 256)))
    
