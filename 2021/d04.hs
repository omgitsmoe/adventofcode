import System.IO
import Data.List(foldl', transpose, sortBy)

-- -- split a string at char c into a list of strings
-- split :: Char -> [Char] -> [Char] -> [[Char]] -> [[Char]]
-- split c [] cur list = if cur /= [] then list ++ [cur] else list
-- split c (x:xs) cur list
--     | x == c = split c xs [] (list ++ [cur])
--     | otherwise = split c xs (cur ++ [x]) list

-- adapted from Prelude's words
split   :: Char -> String -> [String]
-- dropWhile drops elements from the list while the predicate holds true
split c s =  case dropWhile (== c) s of
                      "" -> []
                      s' -> part : split c s''
                            -- break: creates a tuple of two lists from the
                            -- original one separated at condition boundary
                            -- Input: break (3==) [1,2,3,4,5]
                            -- Output: ([1,2],[3,4,5])
                            where (part, s'') = break (== c) s'

-- determine the number of draws it takes to reach a winning status for the
-- board and return the board with the marked fields of drawn numbers as well
-- NOTE: prob very inefficient since the board is recreated 2x per drawn number
-- but I don't know how else this would work in a functional style wihout
-- using a monad
boardWinAfter :: Int -> [String] -> [[String]] -> (Int, [[String]])
-- no win
boardWinAfter nthDraw [] board = (-1, board)
boardWinAfter nthDraw (nr:xs) board
    | won == True = (nthDraw + 1, marked)
    | otherwise = boardWinAfter (nthDraw + 1) xs marked
    where
        -- task description only mentions row or column as win condition, not diagonals!
        -- (while the bingo wiki description does)
        -- so we're only checking those
        -- mark numbers matching our current drawn number with X
        -- TODO does this recreate the entire list every time or can the compiler optimize
        -- this into just changing the specific elements?
        marked = map (map (\s -> if s == nr then "X" else s)) board
        -- could also flatten the board and then check every nth entry to check the cols
        -- instead of using transpose
        cols = transpose marked
        checkWin = any (\x -> all (== "X") x)
        won = checkWin marked || checkWin cols

main = do
    contents <- readFile "d04.in"
    let (numbers_line:board_lines) = lines contents
        numbers = split ',' numbers_line
        -- split the board_lines into into its separate boards:
        -- (general approach, could also assume a 5x5 grid)
        -- foldl iterates over the list elements while providing an accumulator
        -- parameter that can be used to track state
        -- regular foldl can apparently be slow so use foldl'
        -- use let without binding a name so we can give the params a type signature when using a lambda
        -- ':' prepends an element to a list y:x:[] -> [y, x]
        -- '++' concats lists: [y] ++ [x] -> [y, x]
        -- '++' has to traverse the lhs list completely and thus can be slow
        -- so you can either:
        -- 1. prepend the elements with ':' O(1) and then reverse O(n) once at the end
        --    (vs. O(n) for each ++)
        -- 2. use a difference list (where the list is a function that just prepends
        --    the current list to the next [1, 2, 3] would be (\xs -> [1, 2, 3] ++ xs)
        -- 3. use Sequence from Data.Sequence
        -- USE 1. and prepend both individual lines as well as a completed board
        -- and then reverse at the end
        boards = reverse $ snd $ foldl' (\acc x -> let _ = (acc::([String], [[[String]]])); _ = (x::String) in
                                       if x /= "" && any (/= ' ') x
                                           then case acc of
                                                    (cur, all) -> (x:cur, all)
                                           else case acc of
                                                    ([], all) -> ([], all)
                                                    -- reverse the linse of the current board since we
                                                    -- prended them, then extract the fields/words of
                                                    -- the lines
                                                    (cur, all) -> ([], (map words $ reverse cur):all)
                        ) ([]::[String], []) board_lines
        -- get board that wins first
        -- compute after how many draws a board wins then sort them by that number in ascending order
        -- (nth draw, winning board with marked numbers)
        winAfterDraws = sortBy (\(a, _) (b, _) -> compare a b)
            $ map (\b -> boardWinAfter 0 numbers b) boards
        firstWinner = head winAfterDraws
        lastWinner = last winAfterDraws
        numberThatCausedWinFirst = (read::String->Int) $ numbers !! (fst firstWinner - 1)
        numberThatCausedWinLast = (read::String->Int) $ numbers !! (fst lastWinner - 1)
        getSumOfNonMarkedNumbers =
            -- map the strings to ints then sum them up the line then sum up the all the lines
            sum . map (sum . (map (\cell -> if cell == "X" then 0 else (read::String->Int) cell)))
        firstWinnerUnmarked = getSumOfNonMarkedNumbers $ snd firstWinner
        lastWinnerUnmarked = getSumOfNonMarkedNumbers $ snd lastWinner

    putStrLn $ "Part1: " ++ (show $ firstWinnerUnmarked * numberThatCausedWinFirst)
    putStrLn $ "Part2: " ++ (show $ lastWinnerUnmarked * numberThatCausedWinLast)

