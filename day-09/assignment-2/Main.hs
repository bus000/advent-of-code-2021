{- Next, you need to find the largest basins so you know what areas are most
 - important to avoid.
 -
 - A basin is all locations that eventually flow downward to a single low point.
 - Therefore, every low point has a basin, although some basins are very small.
 - Locations of height 9 do not count as being in any basin, and all other
 - locations will always be part of exactly one basin.
 -
 - The size of a basin is the number of locations within the basin, including
 - the low point. The example above has four basins.
 -
 - The top-left basin, size 3:
 -
 -    2199943210
 -    3987894921
 -    9856789892
 -    8767896789
 -    9899965678
 -
 - The top-right basin, size 9:
 -
 -    2199943210
 -    3987894921
 -    9856789892
 -    8767896789
 -    9899965678
 -
 - The middle basin, size 14:
 -
 -    2199943210
 -    3987894921
 -    9856789892
 -    8767896789
 -    9899965678
 -
 - The bottom-right basin, size 9:
 -
 -    2199943210
 -    3987894921
 -    9856789892
 -    8767896789
 -    9899965678
 -
 - Find the three largest basins and multiply their sizes together. In the above
 - example, this is 9 * 14 * 9 = 1134.
 -
 - What do you get if you multiply together the sizes of the three largest
 - basins?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Control.Monad.State as M
import qualified Data.List as L

main :: IO ()
main = defaultMain parseInput handleInput

type HeightMap = Map.Map (Int, Int) Int

handleInput :: HeightMap -> IO ()
handleInput heightMap
    = print
    . product
    . take 3
    . L.sortBy (flip compare)
    . map length
    . map (enumerateBasin heightMap)
    . minima
    $ heightMap

fromRows :: [[Int]] -> HeightMap
fromRows rows = Map.fromList $ do
    (rowN, row) <- zip [0..] rows
    (colN, v) <- zip [0..] row

    return ((colN, rowN), v)

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

enumerateBasin :: HeightMap -> (Int, Int) -> [(Int, Int)]
enumerateBasin heightMap start = M.evalState (go start) Set.empty
  where
    go :: (Int, Int) -> M.State (Set.Set (Int, Int)) [(Int, Int)]
    go pos@(x, y) = do
        seen <- M.get
        let inBasin = Maybe.maybe False (< 9) $ Map.lookup pos heightMap
            seen' = Set.insert pos seen

        if pos `Set.member` seen || not inBasin
            then return []
            else do
                M.put seen'

                lefts <- go (x - 1, y)
                rights <- go (x + 1, y)
                downs <- go (x, y - 1)
                ups <- go (x, y + 1)

                return $ [pos] ++ lefts ++ rights ++ downs ++ ups

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
