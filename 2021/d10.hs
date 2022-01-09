import System.IO
import Data.List(sort)

import Util

main = do
    contents <- readFile $ "d10.in"
    let
        input = lines . trim $ contents
        getPts :: Char -> Int
        getPts ')' = 3
        getPts ']' = 57
        getPts '}' = 1197
        getPts '>' = 25137
        getPts _ = error "Unrecognized char"
        -- returns (points for part1, list of closers need to complete an incomplete line)
        checkLine :: String -> (Int, [Char])
        checkLine l = go [] l
            where
                go closers [] = if not (null closers)
                    -- unbalanced/too few closers
                    -- get first offending
                    then (0, closers) -- NOTE: ignore for part1
                    else (0, [])
                -- go over all the characters
                go closers (c:cs) = case c of
                    -- found opener -> put new closer on stack
                    '(' -> go (')':closers) cs
                    '[' -> go (']':closers) cs
                    '{' -> go ('}':closers) cs
                    '<' -> go ('>':closers) cs
                    foundCloser
                        -- found closer -> check if it matches the one in the closer stack
                        -- use null to check for empty lists since closers == [] only works for
                        -- types that have the typeclass Eq
                        | foundCloser `elem` [')', ']', '}', '>'] -> if null closers
                            -- unbalanced/too many closers
                            then error "Should not happen" -- according to task descr
                            else if expCloser == foundCloser
                                then go remClosers cs
                                -- wrong closer
                                else (getPts foundCloser, [])
                        -- otherwise is just an alias for True
                        | otherwise -> error "Unrecognized char"
                    where
                        (expCloser:remClosers) = closers
        -- could've been done with foldl'
        getClosingScore [] = 0
        getClosingScore l  = go 0 l
            where
                go score [] = score
                go score (x:xs) = go (score * 5 + charScore) xs
                    where charScore = case x of
                            ')' -> 1
                            ']' -> 2
                            '}' -> 3
                            '>' -> 4
        -- filter checline results only keeping incomplete lines (score 0 and closer list not empty)
        -- calc closing score and sort
        scores =
            sort
            . map (getClosingScore . snd)
            . filter (\(p, cs) -> not (null cs) && p == 0)
            . map checkLine
            $ input
        -- scores list length is guaranteed to be uneven
        middleScore = scores !! (((length scores) - 1) `div` 2)

    putStrLn $ "Part1: " ++ (show . sum . map (\l -> fst $ checkLine l) $ input)
    putStrLn $ "Part2: " ++ (show middleScore)
