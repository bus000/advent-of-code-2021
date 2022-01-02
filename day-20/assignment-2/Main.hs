{- You still can't quite make out the details in the image. Maybe you just
 - didn't enhance it enough.
 -
 - If you enhance the starting input image in the above example a total of 50
 - times, 3351 pixels are lit in the final output image.
 -
 - Start again with the original input image and apply the image enhancement
 - algorithm 50 times. How many pixels are lit in the resulting image?
 -}
{-# LANGUAGE BinaryLiterals #-}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Set as Set
import qualified Data.Bits as Bits
import qualified Data.List as L
import qualified Data.Bool as Bool
import qualified Text.Parsec as P
import qualified Control.Monad as M

main :: IO ()
main = defaultMain parseInput handleInput

data Point = Point
    { _x :: !Int
    , _y :: !Int
    } deriving (Show, Eq, Ord)

data Picture = Picture
    { _on     :: !Bool -- Whether pixels stored here are on or off.
    , _pixels :: !(Set.Set Point) -- Pixels.
    } deriving (Show, Eq, Ord)

type Algorithm = V.Vector Bool

handleInput :: (Algorithm, Picture) -> IO ()
handleInput (algorithm, picture)
    = print
    . Set.size
    . _pixels
    . head
    . drop 50
    . iterate (step algorithm)
    $ picture

showPicture :: Picture -> String
showPicture (Picture on pixels)
    | Set.null pixels = ""
    | otherwise = init $ do
        y <- [minY..maxY]
        x <- [minX..maxX+1]
        let char = if Point x y `Set.member` pixels then inChar else outChar

        if x == maxX + 1
            then return '\n'
            else return char
  where
    outChar = if on then '.' else '#'
    inChar = if on then '#' else '.'
    minX = minimum . map _x . Set.elems $ pixels
    maxX = maximum . map _x . Set.elems $ pixels
    minY = minimum . map _y . Set.elems $ pixels
    maxY = maximum . map _y . Set.elems $ pixels

step :: Algorithm -> Picture -> Picture
step algorithm picture@(Picture on pixels) = Picture on' pixels'
  where
    on' = not $ algorithm V.! (if on then 0b0 else 0b111111111)
    pixels' = Set.filter p toConsider
    p = (== on') . (\x -> algorithm V.! x) . lookupBits picture . neighbourhood

    toConsider = Set.fold expand Set.empty pixels
    expand pixel = Set.union (Set.fromList (neighbourhood pixel))

neighbourhood :: Point -> [Point]
neighbourhood (Point x y) =
    [ Point (x - 1) (y - 1), Point (x + 0) (y - 1), Point (x + 1) (y - 1)
    , Point (x - 1) (y + 0), Point (x + 0) (y + 0), Point (x + 1) (y + 0)
    , Point (x - 1) (y + 1), Point (x + 0) (y + 1), Point (x + 1) (y + 1)
    ]

lookupBits :: Picture -> [Point] -> Int
lookupBits (Picture on pixels) = L.foldl' go 0
  where
    go bits pixel = (bits `Bits.shift` 1) + (Bool.bool 0 1 $ lookupBit pixel)

    lookupBit pixel = if pixel `Set.member` pixels then on else not on

fromRows :: [[Bool]] -> Picture
fromRows rows = Picture True . Set.fromList $ do
    (row, rowN) <- zip rows [0..]
    (element, colN) <- zip row [0..]
    M.guard element -- Keep only 1s.
    return $ Point colN rowN

parseInput :: T.Text -> Either P.ParseError (Algorithm, Picture)
parseInput = P.parse (parseProblem <* P.eof) ""

parseProblem :: P.Parsec T.Text () (Algorithm, Picture)
parseProblem = (,) <$> parseAlgorithm <* P.string "\n\n" <*> parsePicture

parseAlgorithm :: P.Parsec T.Text () Algorithm
parseAlgorithm = V.fromList <$> parseRow

parsePicture :: P.Parsec T.Text () Picture
parsePicture = fromRows <$> parseRow `P.endBy` P.newline

parseRow :: P.Parsec T.Text () [Bool]
parseRow = map f <$> P.many1 (P.oneOf ".#")
  where
    f '.' = False
    f '#' = True
    f err = error $ "Unexpected " ++ show err
