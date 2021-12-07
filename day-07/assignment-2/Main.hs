{- The crabs don't seem interested in your proposed solution. Perhaps you
 - misunderstand crab engineering?
 -
 - As it turns out, crab submarine engines don't burn fuel at a constant rate.
 - Instead, each change of 1 step in horizontal position costs 1 more unit of
 - fuel than the last: the first step costs 1, the second step costs 2, the
 - third step costs 3, and so on.
 -
 - As each crab moves, moving further becomes more expensive. This changes the
 - best horizontal position to align them all on; in the example above, this
 - becomes 5:
 -
 - * Move from 16 to 5: 66 fuel
 - * Move from 1 to 5: 10 fuel
 - * Move from 2 to 5: 6 fuel
 - * Move from 0 to 5: 15 fuel
 - * Move from 4 to 5: 1 fuel
 - * Move from 2 to 5: 6 fuel
 - * Move from 7 to 5: 3 fuel
 - * Move from 1 to 5: 10 fuel
 - * Move from 2 to 5: 6 fuel
 - * Move from 14 to 5: 45 fuel
 -
 - This costs a total of 168 fuel. This is the new cheapest possible outcome;
 - the old alignment position (2) now costs 206 fuel instead.
 -
 - Determine the horizontal position that the crabs can align to using the least
 - fuel possible so they can make you an escape route! How much fuel must they
 - spend to align to that position?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Data.Vector as V

main :: IO ()
main = defaultMain parseInput handleInput

handleInput :: (V.Vector Int) -> IO ()
handleInput ns = print $ travelDistance ns (average ns)

travelDistance :: V.Vector Int -> Int -> Int
travelDistance ns target = V.foldl' (+) 0 distances
  where
    distances = V.map (crapDist target) ns

crapDist :: Int -> Int -> Int
crapDist x y = sum [1..abs (x - y)]

average :: V.Vector Int -> Int
average ns = totalSum `div` (V.length ns)
  where
    totalSum = V.foldl' (+) 0 ns

parseInput :: T.Text -> Either P.ParseError (V.Vector Int)
parseInput = P.parse (parseInts <* P.eof) ""

parseInts :: P.Parsec T.Text () (V.Vector Int)
parseInts = V.fromList <$> P.int `P.sepBy` (P.char ',') <* P.newline
