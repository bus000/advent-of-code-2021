{- Next, you should verify the life support rating, which can be determined by
 - multiplying the oxygen generator rating by the CO2 scrubber rating.
 -
 - Both the oxygen generator rating and the CO2 scrubber rating are values that
 - can be found in your diagnostic report - finding them is the tricky part.
 - Both values are located using a similar process that involves filtering out
 - values until only one remains. Before searching for either rating value,
 - start with the full list of binary numbers from your diagnostic report and
 - consider just the first bit of those numbers. Then:
 -
 - * Keep only numbers selected by the bit criteria for the type of rating value
 -   for which you are searching. Discard numbers which do not match the bit
 -   criteria.
 - * If you only have one number left, stop; this is the rating value for which
 -   you are searching.
 - * Otherwise, repeat the process, considering the next bit to the right.
 -
 - The bit criteria depends on which type of rating value you want to find:

 - * To find oxygen generator rating, determine the most common value (0 or 1)
 -   in the current bit position, and keep only numbers with that bit in that
 -   position. If 0 and 1 are equally common, keep values with a 1 in the
 -   position being considered.
 - * To find CO2 scrubber rating, determine the least common value (0 or 1) in
 -   the current bit position, and keep only numbers with that bit in that
 -   position. If 0 and 1 are equally common, keep values with a 0 in the
 -   position being considered.
 -
 - For example, to determine the oxygen generator rating value using the same
 - example diagnostic report from above:
 -
 - * Start with all 12 numbers and consider only the first bit of each number.
 -   There are more 1 bits (7) than 0 bits (5), so keep only the 7 numbers with
 -   a 1 in the first position: 11110, 10110, 10111, 10101, 11100, 10000, and
 -   11001.
 - * Then, consider the second bit of the 7 remaining numbers: there are more 0
 -   bits (4) than 1 bits (3), so keep only the 4 numbers with a 0 in the second
 -   position: 10110, 10111, 10101, and 10000.
 - * In the third position, three of the four numbers have a 1, so keep those
 -   three: 10110, 10111, and 10101.
 - * In the fourth position, two of the three numbers have a 1, so keep those
 -   two: 10110 and 10111.
 - * In the fifth position, there are an equal number of 0 bits and 1 bits (one
 -   each). So, to find the oxygen generator rating, keep the number with a 1 in
 -   that position: 10111.
 - * As there is only one number left, stop; the oxygen generator rating is
 -   10111, or 23 in decimal.
 -
 - Then, to determine the CO2 scrubber rating value from the same example above:
 -
 - * Start again with all 12 numbers and consider only the first bit of each
 -   number. There are fewer 0 bits (5) than 1 bits (7), so keep only the 5
 -   numbers with a 0 in the first position: 00100, 01111, 00111, 00010, and
 -   01010.
 - * Then, consider the second bit of the 5 remaining numbers: there are fewer 1
 -   bits (2) than 0 bits (3), so keep only the 2 numbers with a 1 in the second
 -   position: 01111 and 01010.
 - * In the third position, there are an equal number of 0 bits and 1 bits (one
 -   each). So, to find the CO2 scrubber rating, keep the number with a 0 in
 -   that position: 01010.
 - * As there is only one number left, stop; the CO2 scrubber rating is 01010,
 -   or 10 in decimal.
 -
 - Finally, to find the life support rating, multiply the oxygen generator
 - rating (23) by the CO2 scrubber rating (10) to get 230.
 -
 - Use the binary numbers in your diagnostic report to calculate the oxygen
 - generator rating and CO2 scrubber rating, then multiply them together. What
 - is the life support rating of the submarine? (Be sure to represent your
 - answer in decimal, not binary.)
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Data.Bits as B
import qualified Data.Vector as V
import qualified Data.List as L

main :: IO ()
main = defaultMain parseInput handleInput

handleInput :: [V.Vector Int] -> IO ()
handleInput ns = print $ ogr * csr
  where
    ogr = vToI . filterRating False $ ns
    csr = vToI . filterRating True $ ns

filterRating :: Bool -> [V.Vector Int] -> V.Vector Int
filterRating flipMask ns = case L.foldl' (step flipMask) ns [0..11] of
    [n] -> n
    err -> error $ "Should not happen" ++ show err

step :: Bool -> [V.Vector Int] -> Int -> [V.Vector Int]
step _ [n] _ = [n]
step flipMask ns i = if mask
    then filter (\v -> v V.! i == 1) ns
    else  filter (\v -> v V.! i == -1) ns
  where
    realMask = (\v -> v V.! i) . V.map (>= 0) . vSum $ ns
    mask = if flipMask then not realMask else realMask

vSum :: [V.Vector Int] -> V.Vector Int
vSum ns = foldr (V.zipWith (+)) zeros ns
  where
    zeros = V.fromList [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

vToI :: V.Vector Int -> Int
vToI v = V.ifoldl' f (0 :: Int) (V.reverse v')
  where 
    v' = V.map (> 0) v
    f n i True = B.setBit n i
    f n i False = B.clearBit n i

parseInput :: T.Text -> Either P.ParseError [V.Vector Int]
parseInput = P.parse (parseNumbers <* P.eof) ""

parseNumbers :: P.Parsec T.Text () [V.Vector Int]
parseNumbers = parseNumber `P.endBy` P.newline

parseNumber :: P.Parsec T.Text () (V.Vector Int)
parseNumber = V.fromList . map toInt <$> P.count 12 P.binDigit
  where
    toInt '0' = -1
    toInt '1' = 1
    toInt _ = error "Unexpected"
