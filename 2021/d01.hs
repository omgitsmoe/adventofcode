import System.IO

-- function type: taking a list of type a where a fills? the Num constraint
-- and Integer as return type
countIncreasing :: (Ord a) => [a] -> Integer
-- pattern matching: empty list ret 0; singleton list 0 etc.
countIncreasing [] = 0
countIncreasing [x] = 0
countIncreasing [x, y] = if y > x then 1 else 0
countIncreasing (x:y:xs)
    | y > x = 1 + lastCount
    | otherwise = lastCount
    -- recursive call with same list minus it's head; lastCount is our accumulator
    -- need $ here again so prepend with : is done before passing it to countIncreasing
    where lastCount = countIncreasing $ y:xs

-- produce a list of the sum of each moving window of size 3 over the the original list
windowDepths :: (Num a) => [a] -> [a]
windowDepths xs
    -- stop when we don't have a full window anymore
    | length xs < 3 = []
    -- take the first 3 elements and sum them
    -- prepend to the recursive call of the same list with the window starting at
    -- the next element (moving window)
    | otherwise = sum (take 3 xs) : windowDepths (drop 1 xs)

main = do
    -- use '<-' to bind IO operation
    contents <- readFile "d01.in"
    -- split by newline with 'lines'
    -- use $ to get lower precedence so lines gets evaluated first or could use ()
    -- ($ has lowest possible precedence of all infix operators, and so when you see it
    -- in an expression it is evaluated last)
    -- filter out empty lines by passing a predicat lambda as our first parameter
    -- it tests whether there is any char that is not equal to a ' ' in line l
    -- convert the line to an int with read and by specifying function type
    -- apply that to all the lines using map
    let depths = map (read::String->Integer) $ filter (\l -> any (/= ' ') l) $ lines contents
    -- write stdout line and use show convert the int to str
    -- use function composition operator '.' to apply show to the result of countIncreasing
    -- with the only parameter depths
    putStrLn $ "Part1: " ++ (show . countIncreasing) depths
    putStr "Part2: "
    -- using function composition again: these are applied from right to left with first
    -- param being 'depths' and each function call to the left using the result of the
    -- previous function
    (putStrLn . show . countIncreasing . windowDepths) depths
    -- return is not a return in the imperative sense, instead it wraps
    -- whatever object on the rhs in an IO operation?
    return ()
    
