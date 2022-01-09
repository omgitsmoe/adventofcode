import System.IO
import Data.List
import Numeric(readInt)
import Data.Maybe(listToMaybe, fromJust)
import Data.Char(digitToInt)
import Data.Function((&))

-- well haskell doesn't have a binlit->int function....
readBin :: Integral a => String -> Maybe a
readBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

-- mostCommonBits =
--         -- Multiline expressions in do syntax must be indented beyond the variable name
--         -- no compiler warning for that btw.
--         -- if there are more 1/set bits than the half the bit amount 1 will be the most common one
--         map (\set_bits -> if set_bits > nrBitsHalf then '1' else '0')
--         -- iterate over the strings converting them to ints and summing the up
--         $ map (\bitstring -> sum $ map (\c -> if c == '1' then 1 else 0) bitstring)
--         -- use transpose to convert the rows to columns so we have the 
--         $ (transpose input)
-- this is nicer to read with haskell's equivalent of a pipe operator
--    mostCommonBits =
--        transpose input
--            & map (\bitstring -> sum $ map (\c -> if c == '1' then 1 else 0) bitstring)
--            -- use >= here so we choose 1 if equal for pt2
--            & map (\set_bits -> if set_bits >= nrBitsHalf then '1' else '0')
getMostCommonBits input =
    transpose input
        -- tried for a really long time for map to use read::Char->Int
        -- but it just wouldn't work and result in a type error
        -- even though this function does the same in terms of Char->Int
        & map (\bitstring -> sum $ map (\c -> if c == '1' then 1 else 0) bitstring)
        -- use >= here so we choose 1 if equal for pt2
        & map (\set_bits -> if set_bits >= nrBitsHalf then '1' else '0')
          -- threshold of bits set where the 1 bit becomes the most common one
          -- need to use the length of input / nr of rows since the cols gets transposed to rows
    where nrBitsHalf = ceiling $ ((fromIntegral::Int->Float) $ length input) / 2.0

pickNumbers :: Bool -> Int -> [String] -> (Int, [String])
pickNumbers most idx [] = (idx, [])
-- so we exit the recursive calls when we have one remaining number which will be the rating
pickNumbers most idx [x] = (idx, [x])
pickNumbers most idx numbers
    | idx < (length $ head numbers) =
        -- recurse filtering out numbers that dont match the matchBit at the current pos
        pickNumbers most (idx + 1) $ filter (\n -> (n !! idx) == matchBit) numbers
    | otherwise = (idx, numbers)
          -- we have to recompute the mostCommonBits every iteration since the numbers change
          -- OMG ^ the task description does not really mention that the most common bits
          -- should be recomputed until the step by step description which I did not read
          -- till laaater on :(
    where mostCommonBit = (getMostCommonBits numbers) !! idx
          -- flip matchBit if we want to pick numbers based on least common one
          -- !!OMG had matchBit here still since I split this up into two definitions and apparently
          -- the compiler doesn't warn about it and just causes and infinite loop and don't
          -- know why that name is even accessible?
          -- matchBit = (if most then matchBit else (if matchBit == '1' then '0' else '1'))
          matchBit = (if most then mostCommonBit else (if mostCommonBit == '1' then '0' else '1'))

main = do
    contents <- readFile "d03.in"
    let input = [l | l <- lines contents, any (/= ' ') l]
        mostCommonBits = getMostCommonBits input
        -- convert to int from binary literal and "unwrap" the Maybe
        decGammaRate = fromJust (readBin mostCommonBits)
        decEpsilonRate = fromJust
            -- reverse bits in mostCommonBits then convert to int
            $ (readBin::[Char]->Maybe Int) $ map (\c -> if c == '1' then '0' else '1') mostCommonBits
        (_, oxygenGenRatingBinLit:_) = pickNumbers True 0 input
        oxygenGenRating = fromJust (readBin oxygenGenRatingBinLit)
        (_, co2ScrubRatingBinLit:_) = pickNumbers False 0 input
        co2ScrubRating = fromJust (readBin co2ScrubRatingBinLit)

    -- this function call / resolution order is so confusing
    -- show (decGammaRate * decEpsilonRate)
    -- ^ works but below doesn't even though $ should be the lowest presedence and the
    -- * shouldve been evaluated before it:
    -- show $ decGammaRate * decEpsilonRate
    putStrLn $ ("Part1: " ++ show (decGammaRate * decEpsilonRate))
    putStrLn $ ("Part2: " ++ show (oxygenGenRating * co2ScrubRating))
    return ()
