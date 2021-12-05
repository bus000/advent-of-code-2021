{- Unfortunately, considering only horizontal and vertical lines doesn't give
 - you the full picture; you need to also consider diagonal lines.
 -
 - Because of the limits of the hydrothermal vent mapping system, the lines in
 - your list will only ever be horizontal, vertical, or a diagonal line at
 - exactly 45 degrees. In other words:
 -
 - * An entry like 1,1 -> 3,3 covers points 1,1, 2,2, and 3,3.
 - * An entry like 9,7 -> 7,9 covers points 9,7, 8,8, and 7,9.
 -
 - Considering all lines from the above example would now produce the following
 - diagram:
 -
 -    1.1....11.
 -    .111...2..
 -    ..2.1.111.
 -    ...1.2.2..
 -    .112313211
 -    ...1.2....
 -    ..1...1...
 -    .1.....1..
 -    1.......1.
 -    222111....
 -
 - You still need to determine the number of points where at least two lines
 - overlap. In the above example, this is still anywhere in the diagram with a 2
 - or larger - now a total of 12 points.
 -
 - Consider all of the lines. At how many points do at least two lines overlap?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Data.Map as Map
import qualified Data.List as L

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
expandLine (Line p1@(Point x1 y1) p2@(Point x2 y2))
    | p1 == p2 = [p1]
    | otherwise = p1:L.unfoldr f p1
  where
    deltaX = x2 - x1
    deltaY = y2 - y1
    normalizer = max (abs deltaX) (abs deltaY)
    dx = deltaX `div` normalizer
    dy = deltaY `div` normalizer

    f p@(Point x y)
        | p == p2 = Nothing
        | otherwise = Just . twice $ Point (x + dx) (y + dy)

twice :: a -> (a, a)
twice x = (x, x)

parseInput :: T.Text -> Either P.ParseError [Line]
parseInput = P.parse (parseLines <* P.eof) ""

parseLines :: P.Parsec T.Text () [Line]
parseLines = parseLine `P.endBy` P.newline

parseLine :: P.Parsec T.Text () Line
parseLine = Line <$> parsePoint <* P.string " -> " <*> parsePoint

parsePoint :: P.Parsec T.Text () Point
parsePoint = Point <$> P.int <* P.char ',' <*> P.int
