module Util
( splitOn
, trim
) where

import qualified Data.Char as Char
import Data.List(dropWhileEnd)

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

trim :: String -> String
trim s = (dropWhile Char.isSpace . dropWhileEnd Char.isSpace) s
