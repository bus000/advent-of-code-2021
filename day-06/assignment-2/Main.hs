{- Suppose the lanternfish live forever and have unlimited food and space. Would
 - they take over the entire ocean?
 -
 - After 256 days in the example above, there would be a total of 26984457539
 - lanternfish!
 -
 - How many lanternfish would there be after 256 days?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Data.List as L
import qualified Data.Vector as V

main :: IO ()
main = defaultMain parseInput handleInput

data Fish = Fish
    { _fish :: V.Vector Integer
    } deriving (Show, Eq, Ord)

handleInput :: Fish -> IO ()
handleInput = print . countFish . head . drop 256 . iterate stepFish

stepFish :: Fish -> Fish
stepFish (Fish ns) = Fish ns'
  where
    ns' = V.fromList
        [ ns V.! 1
        , ns V.! 2
        , ns V.! 3
        , ns V.! 4
        , ns V.! 5
        , ns V.! 6
        , ns V.! 7 + ns V.! 0
        , ns V.! 8
        , ns V.! 0
        ]

countFish :: Fish -> Integer
countFish (Fish ns) = V.foldl' (+) 0 ns

fromList :: [Integer] -> Fish
fromList ns = Fish $ L.foldl' (V.zipWith (+)) zeros vs
  where
    fromInt n = V.generate 9 (\x -> if (fromIntegral x) == n then 1 else 0)
    vs = map fromInt ns
    zeros = V.replicate 9 0

parseInput :: T.Text -> Either P.ParseError Fish
parseInput = P.parse (parseFish <* P.eof) ""

parseFish :: P.Parsec T.Text () Fish
parseFish = fromList <$> P.int `P.sepBy` (P.char ',') <* P.newline
