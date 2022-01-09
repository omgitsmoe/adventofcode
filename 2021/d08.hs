import System.IO
import Data.List(foldl', (\\), sort)
import qualified Data.Map.Strict as Map

import Util

-- [["abc", "be", ..], ["bef", "abd", ..]]
decodeOut :: [[String]] -> Int
decodeOut [] = error "Can't decode empty input"
-- need to sort key, since we built the numbers from the segments ourselves
-- instead of using the ones from the signals
decodeOut [uniqueSignals, out] = (read::String->Int) . map (\s -> seqToDigit Map.! (sort s)) $ out
    where
        -- get known numbers by length
        (knownDigitToStrList, withoutKnowns) = foldl' (\(k, l) s -> case length s of
            2 -> ((1, s):k, l)
            3 -> ((7, s):k, l)
            4 -> ((4, s):k, l)
            7 -> ((8, s):k, l)
            _ -> (k, s:l)) ([], []) uniqueSignals
        knownDigitToStr = Map.fromList knownDigitToStrList
        -- 4 has all the parts that 7 has but the top one
        -- so we can use \\ to build the diff and get the topTop segment
        topTop = head $ (knownDigitToStr Map.! 7) \\ (knownDigitToStr Map.! 4)
        -- diff with 8 or all segments will one segment for 3 numbers: 0, 6, 9
        (topRight, midMid, botLeft) = foldl' (\(t, m, b) s -> case "abcedfg" \\ s of
            [c]
                -- one has topRight but not midMid or botLeft so if c is in 1 we habe topRight
                -- which means the number is
                -- 6
                | c `elem` (knownDigitToStr Map.! 1) -> (c, m, b)
                -- 4 has topRight and midMid segment so this means s will be the represantation
                -- of 0 or 6, but 6 was filtered above so we get midMid here from
                -- 0
                | c `elem` (knownDigitToStr Map.! 4) -> (t, c, b)
                -- get botLeft from 9:
                -- 9
                | otherwise -> (t, m, c)
            _ -> (t, m, b)) (' ', ' ', ' ') withoutKnowns
        -- 4 + botLeft and topTop -> only botBot missing
        botMid = head $ "abcedfg" \\ (botLeft:topTop:(knownDigitToStr Map.! 4))
        -- construct segments with only topLeft missing starting from 7 and build diff from 8/all
        topLeft = head $ "abcdefg" \\ (midMid:botLeft:botMid:(knownDigitToStr Map.! 7))
        botRight = head $ (knownDigitToStr Map.! 4) \\ (topLeft:midMid:topRight:[])
        -- build map from sorted segments to digit it represents (as char)
        seqToDigit = Map.fromList
            -- sort key/segments
            $ map (\(k, v) -> (sort k, v))
                [(topTop:topRight:botRight:botMid:botLeft:topLeft:[], '0'),
                 (knownDigitToStr Map.! 1, '1'),
                 (topTop:topRight:botMid:botLeft:midMid:[], '2'),
                 (topTop:topRight:botRight:botMid:midMid:[], '3'),
                 (knownDigitToStr Map.! 4, '4'),
                 (topTop:botRight:botMid:midMid:topLeft:[], '5'),
                 (topTop:botRight:botMid:botLeft:midMid:topLeft:[], '6'),
                 (knownDigitToStr Map.! 7, '7'),
                 ("abcdefg", '8'),
                 ("abcdefg" \\ [botLeft], '9')]

main = do
    contents <- readFile "d08.in"
    -- known combinations
    -- num segments to digit
    let known = Map.fromList [(2, 1), (3, 7), (4, 4), (7, 8)]
        -- ([unique signal pattern], [output value])
        patternsOutput = map (map words . splitOn " | ") . lines $ contents
        -- count known values by the number of active display segments they use
        countKnowns [] = 0
        countKnowns (x:xs) = if Map.member (length x) known then countKnowns xs + 1 else countKnowns xs
        nrKnownDigits = foldl' (\acc [_, out] -> acc + (countKnowns out)) 0 patternsOutput
        sumOfOutputs = foldl' (\acc x -> acc + (decodeOut x)) 0 patternsOutput

    putStrLn $ "Part1: " ++ (show nrKnownDigits)
    putStrLn $ "Part2: " ++ (show sumOfOutputs)
