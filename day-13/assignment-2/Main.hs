{- Finish folding the transparent paper according to the instructions. The
 - manual says the code is always eight capital letters.
 -
 - What code do you use to activate the infrared thermal imaging camera system?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Data.Set as Set
import qualified Data.List as L

main :: IO ()
main = defaultMain parseInput handleInput

type Point = (Int, Int)

type PointCloud = Set.Set Point

data Fold = Vertical !Int | Horizontal !Int
  deriving (Show, Eq, Ord)

handleInput :: (PointCloud, [Fold]) -> IO ()
handleInput (points, folds) = putStrLn . ppPoints . L.foldl' fold points $ folds

fold :: PointCloud -> Fold -> PointCloud
fold points (Horizontal line) = Set.map f points
  where
    f (x, y) = if y < line then (x, y) else (x, y - 2*(y - line))
fold points (Vertical line) = Set.map f points
  where
    f (x, y) = if x < line then (x, y) else (x - 2*(x - line), y)

ppPoints :: PointCloud -> String
ppPoints points = tail $ do
    y <- [0..maxY]
    x <- [-1,0..maxX]

    if x == -1
        then return '\n'
        else if (x, y) `Set.member` points
            then return '#'
            else return '.'
  where
    maxX = maximum . map fst . Set.elems $ points
    maxY = maximum . map snd . Set.elems $ points

parseInput :: T.Text -> Either P.ParseError (PointCloud, [Fold])
parseInput = P.parse (parseProblem <* P.eof) ""

parseProblem :: P.Parsec T.Text () (PointCloud, [Fold])
parseProblem = (,) <$> parsePoints <* P.newline <*> parseFolds

parsePoints :: P.Parsec T.Text () PointCloud
parsePoints = Set.fromList <$> parsePoint `P.endBy` P.newline

parsePoint :: P.Parsec T.Text () Point
parsePoint = (,) <$> P.int <* P.char ',' <*> P.int

parseFolds :: P.Parsec T.Text () [Fold]
parseFolds = parseFold `P.endBy` P.newline

parseFold :: P.Parsec T.Text () Fold
parseFold = P.string "fold along " *> P.choice [vert, hort]
  where
    vert = Vertical <$> (P.string "x=" *> P.int)
    hort = Horizontal <$> (P.string "y=" *> P.int)
