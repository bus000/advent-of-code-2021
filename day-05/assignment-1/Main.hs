{- --- Day 5: Hydrothermal Venture ---
 -
 - You come across a field of hydrothermal vents on the ocean floor! These vents
 - constantly produce large, opaque clouds, so it would be best to avoid them if
 - possible.
 -
 - They tend to form in lines; the submarine helpfully produces a list of nearby
 - lines of vents (your puzzle input) for you to review. For example:
 -
 -    0,9 -> 5,9
 -    8,0 -> 0,8
 -    9,4 -> 3,4
 -    2,2 -> 2,1
 -    7,0 -> 7,4
 -    6,4 -> 2,0
 -    0,9 -> 2,9
 -    3,4 -> 1,4
 -    0,0 -> 8,8
 -    5,5 -> 8,2
 -
 - Each line of vents is given as a line segment in the format x1,y1 -> x2,y2
 - where x1,y1 are the coordinates of one end the line segment and x2,y2 are the
 - coordinates of the other end. These line segments include the points at both
 - ends. In other words:
 -
 - * An entry like 1,1 -> 1,3 covers points 1,1, 1,2, and 1,3.
 - * An entry like 9,7 -> 7,7 covers points 9,7, 8,7, and 7,7.
 -
 - For now, only consider horizontal and vertical lines: lines where either
 - x1 = x2 or y1 = y2.
 -
 - So, the horizontal and vertical lines from the above list would produce the
 - following diagram:
 -
 -    .......1..
 -    ..1....1..
 -    ..1....1..
 -    .......1..
 -    .112111211
 -    ..........
 -    ..........
 -    ..........
 -    ..........
 -    222111....
 -
 - In this diagram, the top left corner is 0,0 and the bottom right corner is
 - 9,9. Each position is shown as the number of lines which cover that point or
 - . if no line covers that point. The top-left pair of 1s, for example, comes
 - from 2,2 -> 2,1; the very bottom row is formed by the overlapping lines 0,9
 - -> 5,9 and 0,9 -> 2,9.
 -
 - To avoid the most dangerous areas, you need to determine the number of points
 - where at least two lines overlap. In the above example, this is anywhere in
 - the diagram with a 2 or larger - a total of 5 points.
 -
 - Consider only horizontal and vertical lines. At how many points do at least
 - two lines overlap?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Data.Map as Map

main :: IO ()
main = defaultMain parseInput handleInput

data Point = Point
    { _x :: !Int
    , _y :: !Int
    } deriving (Show, Eq, Ord)

data Line = Line
    { _start :: !Point
    , _end   :: !Point
    } deriving (Show, Eq, Ord)

handleInput :: [Line] -> IO ()
handleInput
    = print
    . length
    . filter (> 1)
    . Map.elems
    . Map.fromListWith (+)
    . map (\x -> (x, 1 :: Int))
    . concatMap expandLine

expandLine :: Line -> [Point]
expandLine l@(Line (Point x1 y1) (Point x2 y2))
    | isHorizontal l = map (`Point` y1) [min x1 x2..max x1 x2]
    | isVertical l = map (Point x1) [min y1 y2..max y1 y2]
    | otherwise = []

isHorizontal :: Line -> Bool
isHorizontal (Line (Point _ y1) (Point _ y2)) = y1 == y2

isVertical :: Line -> Bool
isVertical (Line (Point x1 _) (Point x2 _)) = x1 == x2

parseInput :: T.Text -> Either P.ParseError [Line]
parseInput = P.parse (parseLines <* P.eof) ""

parseLines :: P.Parsec T.Text () [Line]
parseLines = parseLine `P.endBy` P.newline

parseLine :: P.Parsec T.Text () Line
parseLine = Line <$> parsePoint <* P.string " -> " <*> parsePoint

parsePoint :: P.Parsec T.Text () Point
parsePoint = Point <$> P.int <* P.char ',' <*> P.int
