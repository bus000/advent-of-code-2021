{- --- Day 9: Smoke Basin ---
 -
 - These caves seem to be lava tubes. Parts are even still volcanically active;
 - small hydrothermal vents release smoke into the caves that slowly settles
 - like rain.
 -
 - If you can model how the smoke flows through the caves, you might be able to
 - avoid it and be that much safer. The submarine generates a heightmap of the
 - floor of the nearby caves for you (your puzzle input).
 -
 - Smoke flows to the lowest point of the area it's in. For example, consider
 - the following heightmap:
 -
 -    2199943210
 -    3987894921
 -    9856789892
 -    8767896789
 -    9899965678
 -
 - Each number corresponds to the height of a particular location, where 9 is
 - the highest and 0 is the lowest a location can be.
 -
 - Your first goal is to find the low points - the locations that are lower than
 - any of its adjacent locations. Most locations have four adjacent locations
 - (up, down, left, and right); locations on the edge or corner of the map have
 - three or two adjacent locations, respectively. (Diagonal locations do not
 - count as adjacent.)
 -
 - In the above example, there are four low points, all highlighted: two are in
 - the first row (a 1 and a 0), one is in the third row (a 5), and one is in the
 - bottom row (also a 5). All other locations on the heightmap have some lower
 - adjacent location, and so are not low points.
 -
 - The risk level of a low point is 1 plus its height. In the above example, the
 - risk levels of the low points are 2, 1, 6, and 6. The sum of the risk levels
 - of all low points in the heightmap is therefore 15.
 -
 - Find all of the low points on your heightmap. What is the sum of the risk
 - levels of all low points on your heightmap?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

main :: IO ()
main = defaultMain parseInput handleInput

type HeightMap = Map.Map (Int, Int) Int

handleInput :: HeightMap -> IO ()
handleInput = print . totalRisk

fromRows :: [[Int]] -> HeightMap
fromRows rows = Map.fromList $ do
    (rowN, row) <- zip [0..] rows
    (colN, v) <- zip [0..] row

    return ((colN, rowN), v)

totalRisk :: HeightMap -> Int
totalRisk heightMap = sum . map (riskLevel heightMap) . minima $ heightMap

minima :: HeightMap -> [(Int, Int)]
minima heightMap = filter (isMinimum heightMap) . Map.keys $ heightMap

isMinimum :: HeightMap -> (Int, Int) -> Bool
isMinimum heightMap (x, y) =
    value < up && value < down && value < left && value < right
  where
    value = Maybe.maybe 10 id . Map.lookup (x, y) $ heightMap
    up = Maybe.maybe 10 id . Map.lookup (x + 1, y) $ heightMap
    down = Maybe.maybe 10 id . Map.lookup (x - 1, y) $ heightMap
    left = Maybe.maybe 10 id . Map.lookup (x, y + 1) $ heightMap
    right = Maybe.maybe 10 id . Map.lookup (x, y - 1) $ heightMap

riskLevel :: HeightMap -> (Int, Int) -> Int
riskLevel heightMap pos = case Map.lookup pos heightMap of
    Just value -> value + 1
    Nothing -> 0

parseInput :: T.Text -> Either P.ParseError HeightMap
parseInput = P.parse (parseTable <* P.eof) ""

parseTable :: P.Parsec T.Text () HeightMap
parseTable = fromRows <$> parseRow `P.endBy` P.newline

parseRow :: P.Parsec T.Text () [Int]
parseRow = map getDigit <$> P.many1 P.digit
  where
    getDigit '0' = 0
    getDigit '1' = 1
    getDigit '2' = 2
    getDigit '3' = 3
    getDigit '4' = 4
    getDigit '5' = 5
    getDigit '6' = 6
    getDigit '7' = 7
    getDigit '8' = 8
    getDigit '9' = 9
    getDigit err = error $ "getDigit called with value :" ++ show err
