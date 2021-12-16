{- It seems like the individual flashes aren't bright enough to navigate.
 - However, you might have a better option: the flashes seem to be
 - synchronizing!
 -
 - In the example above, the first time all octopuses flash simultaneously is
 - step 195:
 -
 - After step 193:
 -    5877777777
 -    8877777777
 -    7777777777
 -    7777777777
 -    7777777777
 -    7777777777
 -    7777777777
 -    7777777777
 -    7777777777
 -    7777777777
 -
 - After step 194:
 -    6988888888
 -    9988888888
 -    8888888888
 -    8888888888
 -    8888888888
 -    8888888888
 -    8888888888
 -    8888888888
 -    8888888888
 -    8888888888
 -
 - After step 195:
 -    0000000000
 -    0000000000
 -    0000000000
 -    0000000000
 -    0000000000
 -    0000000000
 -    0000000000
 -    0000000000
 -    0000000000
 -    0000000000
 -
 - If you can calculate the exact moments when the octopuses will all flash
 - simultaneously, you should be able to navigate through the cavern. What is
 - the first step during which all octopuses flash?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Control.Monad as M
import qualified Control.Monad.RWS as RWS
import Util (converge)

main :: IO ()
main = defaultMain parseInput handleInput

data Octopus = Octopus
    { _flashed :: Bool
    , _energy  :: Int
    } deriving (Show, Eq, Ord)

type Octopi = Map.Map (Int, Int) Octopus

handleInput :: Octopi -> IO ()
handleInput octopi = case L.findIndex (== 100) . simulate $ octopi of
    Just step -> print $ step + 1
    Nothing -> error "List is infinite."

simulate :: Octopi -> [Int]
simulate initialOctopi = snd $ RWS.evalRWS go () initialOctopi
  where
    go = M.forever $ do
        octopi <- RWS.get
        let incremented = Map.map incEnergy octopi
            cascaded = cascade incremented
            reset = Map.map resetOctopus cascaded
            lighted = length . filter _flashed . Map.elems $ cascaded
        RWS.put reset
        RWS.tell [lighted]

cascade :: Octopi -> Octopi
cascade = converge go
  where
    go octopi = Map.foldlWithKey' flash octopi octopi

    flash octopi pos val
        | shouldFlash val = Map.insert pos (markFlashed val)
            $ incNeighbours pos octopi
        | otherwise = octopi

    incNeighbours (x, y) octopi = L.foldl' (flip (Map.update incMay)) octopi $
        [ (x - 1, y - 1), (x + 0, y - 1), (x + 1, y - 1)
        , (x - 1, y + 0),                 (x + 1, y + 0)
        , (x - 1, y + 1), (x + 0, y + 1), (x + 1, y + 1)
        ]

    incMay = Just . incEnergy

resetOctopus :: Octopus -> Octopus
resetOctopus (Octopus _ energy)
    | energy > 9 = Octopus False 0
    | otherwise = Octopus False energy

incEnergy :: Octopus -> Octopus
incEnergy (Octopus flashed energy) = Octopus flashed (energy + 1)

shouldFlash :: Octopus -> Bool
shouldFlash (Octopus flashed energy) = not flashed && energy > 9

markFlashed :: Octopus -> Octopus
markFlashed (Octopus _ energy) = Octopus True energy

newOctopus :: Int -> Octopus
newOctopus energy = Octopus False energy

fromRows :: [[Int]] -> Octopi
fromRows rows = Map.fromList $ do
    (rowN, row) <- zip [0..] rows
    (colN, energy) <- zip [0..] row

    return ((rowN, colN), newOctopus energy)

parseInput :: T.Text -> Either P.ParseError Octopi
parseInput = P.parse (parseOctopi <* P.eof) ""

parseOctopi :: P.Parsec T.Text () Octopi
parseOctopi = fromRows <$> parseRow `P.endBy` P.newline

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
