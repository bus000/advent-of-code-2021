{- On the other hand, it might be wise to try a different strategy: let the
 - giant squid win.
 -
 - You aren't sure how many bingo boards a giant squid could play at once, so
 - rather than waste time counting its arms, the safe thing to do is to figure
 - out which board will win last and choose that one. That way, no matter which
 - boards it picks, it will win for sure.
 -
 - In the above example, the second board is the last to win, which happens
 - after 13 is eventually called and its middle column is completely marked. If
 - you were to keep playing until this point, the second board would have a sum
 - of unmarked numbers equal to 148 for a final score of 148 * 13 = 1924.
 -
 - Figure out which board will win last. Once it wins, what would its final
 - score be?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Data.Set as Set
import qualified Data.List as L
import qualified Control.Monad as C

main :: IO ()
main = defaultMain parseInput handleInput

data Game = Game
    { _rng :: [Int]
    , _boards :: [Board] 
    } deriving (Show, Eq)

data Board = Board
    { _rowsAndColumns :: [Set.Set Int]
    } deriving (Show, Eq, Ord)

handleInput :: Game -> IO ()
handleInput game = print $ evaluateWin board ns
  where
    (board, ns) = last $ findWinners game

findWinners :: Game -> [(Board, [Int])]
findWinners game = removeDups $ do
    rng <- L.inits . _rng $ game
    board <- _boards game

    let called = Set.fromList rng
    C.guard $ hasWon called board
    return (board, rng)

removeDups :: Ord a => [(a, b)] -> [(a, b)]
removeDups = reverse . snd . foldl f (Set.empty, [])
  where
    f (seen, output) (key, value) = if key `Set.member` seen
        then (seen, output)
        else (Set.insert key seen, (key, value):output)

evaluateWin :: Board -> [Int] -> Int
evaluateWin board called = sum uncalled * last called
  where
    ns = Set.unions . _rowsAndColumns $ board
    uncalled = Set.difference ns (Set.fromList called)

fromRows :: [[Int]] -> Board
fromRows rows = Board colAndRow
  where
    columns = L.transpose rows
    colAndRow = map Set.fromList rows ++ map Set.fromList columns

hasWon :: Set.Set Int -> Board -> Bool
hasWon called
    = not
    . null
    . filter (== Set.empty)
    . map (\x -> Set.difference x called)
    . _rowsAndColumns

parseInput :: T.Text -> Either P.ParseError Game
parseInput = P.parse (parseGame <* P.eof) ""

parseGame :: P.Parsec T.Text () Game
parseGame = Game <$> parseRng <* P.newline <* P.newline <*> parseBoards

parseRng :: P.Parsec T.Text () [Int]
parseRng = P.int `P.sepBy` (P.char ',')

parseBoards :: P.Parsec T.Text () [Board]
parseBoards = parseBoard `P.sepBy` P.newline

parseBoard :: P.Parsec T.Text () Board
parseBoard = fromRows <$> parseRow `P.endBy` P.newline

parseRow :: P.Parsec T.Text () [Int]
parseRow = P.many (P.char ' ') *> P.int `P.sepBy1` (P.many1 $ P.char ' ')
