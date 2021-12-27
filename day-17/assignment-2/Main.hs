{- Maybe a fancy trick shot isn't the best idea; after all, you only have one
 - probe, so you had better not miss.
 -
 - To get the best idea of what your options are for launching the probe, you
 - need to find every initial velocity that causes the probe to eventually be
 - within the target area after any step.
 -
 - In the above example, there are 112 different initial velocity values that
 - meet these criteria:
 -
 -    23,-10  25,-9   27,-5   29,-6   22,-6   21,-7   9,0     27,-7   24,-5
 -    25,-7   26,-6   25,-5   6,8     11,-2   20,-5   29,-10  6,3     28,-7
 -    8,0     30,-6   29,-8   20,-10  6,7     6,4     6,1     14,-4   21,-6
 -    26,-10  7,-1    7,7     8,-1    21,-9   6,2     20,-7   30,-10  14,-3
 -    20,-8   13,-2   7,3     28,-8   29,-9   15,-3   22,-5   26,-8   25,-8
 -    25,-6   15,-4   9,-2    15,-2   12,-2   28,-9   12,-3   24,-6   23,-7
 -    25,-10  7,8     11,-3   26,-7   7,1     23,-9   6,0     22,-10  27,-6
 -    8,1     22,-8   13,-4   7,6     28,-6   11,-4   12,-4   26,-9   7,4
 -    24,-10  23,-8   30,-8   7,0     9,-1    10,-1   26,-5   22,-9   6,5
 -    7,5     23,-6   28,-10  10,-2   11,-1   20,-9   14,-2   29,-7   13,-3
 -    23,-5   24,-8   27,-9   30,-7   28,-5   21,-10  7,9     6,6     21,-5
 -    27,-10  7,2     30,-9   21,-8   22,-7   24,-9   20,-6   6,9     29,-5
 -    8,-2    27,-8   30,-5   24,-7
 -
 - How many distinct initial velocity values cause the probe to be within the
 - target area after any step?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Control.Monad as M

main :: IO ()
main = defaultMain parseInput handleInput

type Position = (Int, Int)

data Area = Area
    { _xMin :: !Int
    , _xMax :: !Int
    , _yMin :: !Int
    , _yMax :: !Int
    } deriving (Show, Eq, Ord)

handleInput :: Area -> IO ()
handleInput = print . length . possibleVelocities

possibleVelocities :: Area -> [(Int, Int)]
possibleVelocities area@(Area xMin xMax yMin yMax) = do
    dx <- possibleXs area
    dy <- possibleYs area
    M.guard $ hit dx dy
    return (dx, dy)
  where
    hit dx dy = any isHit . takeWhile yBigEnough $ simulate dx dy
    isHit (x, y) = x >= xMin && x <= xMax && y >= yMin && y <= yMax
    yBigEnough (_, y) = y >= yMin

simulate :: Int -> Int -> [(Int, Int)]
simulate initDX initDY = go 0 0 initDX initDY
  where
    go x y dx dy = (x, y) : go (x + dx) (y + dy) (approach0 dx) (dy - 1)
    approach0 n
        | n > 0 = n - 1
        | n == 0 = 0
        | otherwise = n + 1

possibleXs :: Area -> [Int]
possibleXs (Area xMin xMax _ _) = filter p [0..xMax]
  where
    p x = any isBetween . scanl (+) 0 $ [x, x - 1..0]
    isBetween x = x >= xMin && x <= xMax

possibleYs :: Area -> [Int]
possibleYs (Area _ _ yMin yMax)
    | yMin < 0 = filter p $ [0..abs yMin] ++ [-1, -2..yMin]
    | otherwise = filter p [0..yMax]
  where
    p dy = any isBetween . take 1000 . simulateY 0 $ dy
    simulateY y dy = y : simulateY (y + dy) (dy - 1)
    isBetween y = y >= yMin && y <= yMax

maxY :: Area -> Either String Int
maxY (Area _ _ yMin yMax)
    | yMin == 0 = Left "Greatest y is infinite."
    | yMin > 0 = Right yMax
    | otherwise = Right $ (abs yMin) - 1

parseInput :: T.Text -> Either P.ParseError Area
parseInput = P.parse (parseTargetArea <* P.eof) ""

parseTargetArea :: P.Parsec T.Text () Area
parseTargetArea = Area
    <$> (P.string "target area: x=" *> P.int)
    <*> (P.string ".." *> P.int)
    <*> (P.string ", y=" *> P.int)
    <*> (P.string ".." *> P.int <* P.newline)
