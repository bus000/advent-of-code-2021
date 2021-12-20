{- --- Day 15: Chiton ---
 -
 - You've almost reached the exit of the cave, but the walls are getting closer
 - together. Your submarine can barely still fit, though; the main problem is
 - that the walls of the cave are covered in chitons, and it would be best not
 - to bump any of them.
 -
 - The cavern is large, but has a very low ceiling, restricting your motion to
 - two dimensions. The shape of the cavern resembles a square; a quick scan of
 - chiton density produces a map of risk level throughout the cave (your puzzle
 - input). For example:
 -
 -    1163751742
 -    1381373672
 -    2136511328
 -    3694931569
 -    7463417111
 -    1319128137
 -    1359912421
 -    3125421639
 -    1293138521
 -    2311944581
 -
 - You start in the top left position, your destination is the bottom right
 - position, and you cannot move diagonally. The number at each position is its
 - risk level; to determine the total risk of an entire path, add up the risk
 - levels of each position you enter (that is, don't count the risk level of
 - your starting position unless you enter it; leaving it adds no risk to your
 - total).
 -
 - Your goal is to find a path with the lowest total risk. In this example, a
 - path with the lowest total risk is highlighted here:
 -
 -    1163751742
 -    1381373672
 -    2136511328
 -    3694931569
 -    7463417111
 -    1319128137
 -    1359912421
 -    3125421639
 -    1293138521
 -    2311944581
 -
 - The total risk of this path is 40 (the starting position is never entered, so
 - its risk is not counted).
 -
 - What is the lowest total risk of any path from the top left to the bottom
 - right?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.PSQueue as PQ
import Data.PSQueue (Binding((:->)))

main :: IO ()
main = defaultMain parseInput handleInput

type Point = (Int, Int)
data RiskMap = RiskMap
    { _theMap :: !(Map.Map Point Int)
    } deriving (Show, Eq, Ord)

data Distance = Final !Int | Infinity deriving (Show, Eq, Ord)

handleInput :: RiskMap -> IO ()
handleInput = print . Maybe.fromJust . shortestPath (0, 0) (99, 99)

shortestPath :: Point -> Point -> RiskMap -> Maybe Int
shortestPath from to risk
    = Maybe.listToMaybe
    . map snd
    . filter ((== to) . fst)
    . distancesFrom from
    $ risk

distancesFrom :: Point -> RiskMap -> [(Point, Int)]
distancesFrom from (RiskMap risk) = go initialQueue
  where
    go :: PQ.PSQ Point Distance -> [(Point, Int)]
    go queue = case PQ.minView queue of
        Nothing -> []
        Just (_ :-> Infinity, _) -> []
        Just (point :-> Final price, queue') ->
            (point, price) : go (updateNeighbours queue' point price)

    initialQueue :: PQ.PSQ Point Distance
    initialQueue
        = PQ.adjust (const (Final 0)) from
        . PQ.fromList
        . map (:-> Infinity)
        . Map.keys
        $ risk

    updateNeighbours ::
        PQ.PSQ Point Distance -> Point -> Int -> PQ.PSQ Point Distance
    updateNeighbours q point currentPrice =
        L.foldl' (setMinPrice currentPrice) q $ neighbours point

    neighbours :: Point -> [(Point, Int)]
    neighbours (x, y)
        = Maybe.catMaybes
        . map (\point -> Map.lookup point risk >>= \price -> pure (point, price))
        $ [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

    setMinPrice ::
        Int -> PQ.PSQ Point Distance -> (Point, Int) -> PQ.PSQ Point Distance
    setMinPrice currentPrice q (point, price) =
        PQ.adjust (\p -> min p (Final $ currentPrice + price)) point q

fromRows :: [[Int]] -> RiskMap
fromRows rows = RiskMap . Map.fromList $ do
    (row, rowN) <- zip rows [0..]
    (risk, colN) <- zip row [0..]

    return $ ((colN, rowN), risk)

parseInput :: T.Text -> Either P.ParseError RiskMap
parseInput = P.parse (parseMap <* P.eof) ""

parseMap :: P.Parsec T.Text () RiskMap
parseMap = fromRows <$> parseRow `P.endBy` P.newline

parseRow :: P.Parsec T.Text () [Int]
parseRow = map f <$> P.many1 P.digit
  where
    f '0' = 0
    f '1' = 1
    f '2' = 2
    f '3' = 3
    f '4' = 4
    f '5' = 5
    f '6' = 6
    f '7' = 7
    f '8' = 8
    f '9' = 9
    f err = error $ "Unexpected argument " ++ show err
