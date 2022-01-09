import System.IO
import Debug.Trace


-- use algebraic data type to make an enumeration
-- all the value constructors are nullary (take no parameters, i.e. fields) we can make it part
-- of the Enum typeclass
data Direction = Forward | Up | Down
    deriving (Eq, Show, Read, Bounded, Enum)

posChange :: (Direction, Integer) -> (Integer, Integer)
posChange (Forward, change) = (change, 0)
posChange (Up, change) = (0, -change)
posChange (Down, change) = (0, change)

-- all values in haskell are immutable so we can't use sth. like a counter variable
-- use recursion instead
sumTuples :: [(Integer, Integer)] -> (Integer, Integer)
sumTuples [] = (0, 0)
sumTuples (x:xs) = (horizontal + fst x, depth + snd x)
    -- could also use let .. in .. instead, only difference is let bindings are expressions
    -- themselves and they are local so only the current guard (| ... =) can see them
    where (horizontal, depth) = sumTuples xs

withAim :: [(Direction, Integer)] -> (Integer, Integer, Integer) ->
    ([(Direction, Integer)], (Integer, Integer, Integer))
withAim [] state = ([], state)
withAim ((direction, change):xs) (horizontal, depth, aim)
    | direction == Down = withAim xs (horizontal, depth, aim + change)
    | direction == Up = withAim xs (horizontal, depth, aim - change)
    | direction == Forward = withAim xs (horizontal + change, depth + aim * change, aim)
    -- can't really use where, since each of the guards change the state in different ways
    -- but we could curry the function call with the remaining list so we'd only
    -- have to pass the second param in the guard to save some typing but
    -- that'd prob not be worth it
    -- e.g.: | direction == Down = nextCall (horizontal, depth, aim + change)
    --          where nextCall = withAim xs


main = do
    contents <- readFile "d02.in"
    -- list comprehension, split line into words for all lines l in contens where there
    -- is any non-space character
    -- split those lines into words as w, have to use let instead of w <- .. here
    -- otherwise we do this for every word in the line and get forward, 5, up, 3 instead of
    -- [forward, 5], [up, 3]
    -- convert second word into an int
    -- while we pattern match on the first word to get our Direction enum
    let input = [(case head w of "forward" -> Forward
                                 "up" -> Up
                                 "down" -> Down
                  , (read::String->Integer) $ last w) |
                 l <- lines contents, any (/= ' ') l, let w = words l]
        pos = sumTuples $ map posChange input
        -- (0, 0, 0) is starting state
        (_, (horizontal, depth, _)) = withAim input (0, 0, 0)
    putStrLn $ ("Part1: " ++ (show $ fst pos * snd pos))
    putStrLn $ ("Part2: " ++ (show $ horizontal * depth))
