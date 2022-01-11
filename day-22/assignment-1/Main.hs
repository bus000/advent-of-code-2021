{- --- Day 22: Reactor Reboot ---
 -
 - Operating at these extreme ocean depths has overloaded the submarine's
 - reactor; it needs to be rebooted.
 -
 - The reactor core is made up of a large 3-dimensional grid made up entirely of
 - cubes, one cube per integer 3-dimensional coordinate (x,y,z). Each cube can
 - be either on or off; at the start of the reboot process, they are all off.
 - (Could it be an old model of a reactor you've seen before?)
 -
 - To reboot the reactor, you just need to set all of the cubes to either on or
 - off by following a list of reboot steps (your puzzle input). Each step
 - specifies a cuboid (the set of all cubes that have coordinates which fall
 - within ranges for x, y, and z) and whether to turn all of the cubes in that
 - cuboid on or off.
 -
 - For example, given these reboot steps:
 -
 -    on x=10..12,y=10..12,z=10..12
 -    on x=11..13,y=11..13,z=11..13
 -    off x=9..11,y=9..11,z=9..11
 -    on x=10..10,y=10..10,z=10..10
 -
 - The first step (on x=10..12,y=10..12,z=10..12) turns on a 3x3x3 cuboid
 - consisting of 27 cubes:
 -
 - * 10,10,10
 - * 10,10,11
 - * 10,10,12
 - * 10,11,10
 - * 10,11,11
 - * 10,11,12
 - * 10,12,10
 - * 10,12,11
 - * 10,12,12
 - * 11,10,10
 - * 11,10,11
 - * 11,10,12
 - * 11,11,10
 - * 11,11,11
 - * 11,11,12
 - * 11,12,10
 - * 11,12,11
 - * 11,12,12
 - * 12,10,10
 - * 12,10,11
 - * 12,10,12
 - * 12,11,10
 - * 12,11,11
 - * 12,11,12
 - * 12,12,10
 - * 12,12,11
 - * 12,12,12
 -
 - The second step (on x=11..13,y=11..13,z=11..13) turns on a 3x3x3 cuboid that
 - overlaps with the first. As a result, only 19 additional cubes turn on; the
 - rest are already on from the previous step:
 -
 - * 11,11,13
 - * 11,12,13
 - * 11,13,11
 - * 11,13,12
 - * 11,13,13
 - * 12,11,13
 - * 12,12,13
 - * 12,13,11
 - * 12,13,12
 - * 12,13,13
 - * 13,11,11
 - * 13,11,12
 - * 13,11,13
 - * 13,12,11
 - * 13,12,12
 - * 13,12,13
 - * 13,13,11
 - * 13,13,12
 - * 13,13,13
 -
 - The third step (off x=9..11,y=9..11,z=9..11) turns off a 3x3x3 cuboid that
 - overlaps partially with some cubes that are on, ultimately turning off 8
 - cubes:
 -
 - * 10,10,10
 - * 10,10,11
 - * 10,11,10
 - * 10,11,11
 - * 11,10,10
 - * 11,10,11
 - * 11,11,10
 - * 11,11,11
 -
 - The final step (on x=10..10,y=10..10,z=10..10) turns on a single cube,
 - 10,10,10. After this last step, 39 cubes are on.
 -
 - The initialization procedure only uses cubes that have x, y, and z positions
 - of at least -50 and at most 50. For now, ignore cubes outside this region.
 -
 - Here is a larger example:
 -
 - * on x=-20..26,y=-36..17,z=-47..7
 - * on x=-20..33,y=-21..23,z=-26..28
 - * on x=-22..28,y=-29..23,z=-38..16
 - * on x=-46..7,y=-6..46,z=-50..-1
 - * on x=-49..1,y=-3..46,z=-24..28
 - * on x=2..47,y=-22..22,z=-23..27
 - * on x=-27..23,y=-28..26,z=-21..29
 - * on x=-39..5,y=-6..47,z=-3..44
 - * on x=-30..21,y=-8..43,z=-13..34
 - * on x=-22..26,y=-27..20,z=-29..19
 - * off x=-48..-32,y=26..41,z=-47..-37
 - * on x=-12..35,y=6..50,z=-50..-2
 - * off x=-48..-32,y=-32..-16,z=-15..-5
 - * on x=-18..26,y=-33..15,z=-7..46
 - * off x=-40..-22,y=-38..-28,z=23..41
 - * on x=-16..35,y=-41..10,z=-47..6
 - * off x=-32..-23,y=11..30,z=-14..3
 - * on x=-49..-5,y=-3..45,z=-29..18
 - * off x=18..30,y=-20..-8,z=-3..13
 - * on x=-41..9,y=-7..43,z=-33..15
 - * on x=-54112..-39298,y=-85059..-49293,z=-27449..7877
 - * on x=967..23432,y=45373..81175,z=27513..53682
 -
 - The last two steps are fully outside the initialization procedure area; all
 - other steps are fully within it. After executing these steps in the
 - initialization procedure region, 590784 cubes are on.
 -
 - Execute the reboot steps. Afterward, considering only cubes in the region
 - x=-50..50,y=-50..50,z=-50..50, how many cubes are on?
 -}
{-# LANGUAGE DeriveFunctor #-}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Maybe as Maybe
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import Control.Applicative ((<|>))

main :: IO ()
main = defaultMain parseInput handleInput

data Box = Box
    { _xMin :: !Int
    , _xMax :: !Int
    , _yMin :: !Int
    , _yMax :: !Int
    , _zMin :: !Int
    , _zMax :: !Int
    } deriving (Show, Eq, Ord)

data Operation a
    = Add !a
    | Sub !a
  deriving (Show, Eq, Ord, Functor)

type Interval = (Int, Int)

handleInput :: [Operation Box] -> IO ()
handleInput
    = print
    . evaluate
    . map (fmap boxSize)
    . L.foldl' addOperation []
    . Maybe.catMaybes
    . map filter50
  where
    filter50 (Add box) = intersection box50 box >>= return . Add
    filter50 (Sub box) = intersection box50 box >>= return . Sub
    box50 = newBox (-50, 50) (-50, 50) (-50, 50)

addOperation :: [Operation Box] -> Operation Box -> [Operation Box]
addOperation operations operation = case operation of
    Add box -> (Add box):operations'
    Sub _ -> operations'
  where
    operations' = concatMap (f (target operation)) operations
    f box (Add x) = case intersection box x of
        Just same -> [Sub same, Add x]
        Nothing -> [Add x]
    f box (Sub x) = case intersection box x of
        Just same -> [Add same, Sub x]
        Nothing -> [Sub x]

evaluate :: Num a => [Operation a] -> a
evaluate = L.foldl' f 0
  where
    f x (Add y) = x + y
    f x (Sub y) = x - y

-- When do rectangles intersect? When there is an intersection on both axis
-- When do boxes intersect? Hopefully the same way.
--
--      ---------
--      |       |
--      |       |
-- ------------ |
-- |    |     | |
-- |    |     | |
-- |    ------|--
-- |          |
-- ------------
--
--          -----
--          |   |
--          -----
intersection :: Box -> Box -> Maybe Box
intersection box1 box2
    | xMin <= xMax && yMin <= yMax && zMin <= zMax =
        Just $ Box xMin xMax yMin yMax zMin zMax
    | otherwise =
        Nothing
  where
    xMin = max (_xMin box1) (_xMin box2)
    xMax = min (_xMax box1) (_xMax box2)
    yMin = max (_yMin box1) (_yMin box2)
    yMax = min (_yMax box1) (_yMax box2)
    zMin = max (_zMin box1) (_zMin box2)
    zMax = min (_zMax box1) (_zMax box2)

boxSize :: Box -> Int
boxSize box =
    (_xMax box - _xMin box + 1) *
    (_yMax box - _yMin box + 1) *
    (_zMax box - _zMin box + 1)

newBox :: Interval -> Interval -> Interval -> Box
newBox (xMin, xMax) (yMin, yMax) (zMin, zMax) =
    Box xMin xMax yMin yMax zMin zMax

target :: Operation a -> a
target (Add x) = x
target (Sub x) = x

parseInput :: T.Text -> Either P.ParseError [Operation Box]
parseInput = P.parse (parseOperations <* P.eof) ""

parseOperations :: P.Parsec T.Text () [Operation Box]
parseOperations = parseOperation `P.endBy` P.newline

parseOperation :: P.Parsec T.Text () (Operation Box)
parseOperation = P.char 'o' *> (parseAdd <|> parseSub)
  where
    parseAdd = Add <$> (P.string "n " *> parseBox)
    parseSub = Sub <$> (P.string "ff " *> parseBox)

parseBox :: P.Parsec T.Text () Box
parseBox = newBox
    <$> (P.string "x=" *> parseInterval)
    <*> (P.string ",y=" *> parseInterval)
    <*> (P.string ",z=" *> parseInterval)

parseInterval :: P.Parsec T.Text () Interval
parseInterval = (,) <$> P.int <*> (P.string ".." *> P.int)
