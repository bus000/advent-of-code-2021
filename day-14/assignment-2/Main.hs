{- The resulting polymer isn't nearly strong enough to reinforce the submarine.
 - You'll need to run more steps of the pair insertion process; a total of 40
 - steps should do it.
 -
 - In the above example, the most common element is B (occurring 2192039569602
 - times) and the least common element is H (occurring 3849876073 times);
 - subtracting these produces 2188189693529.
 -
 - Apply 40 steps of pair insertion to the polymer template and find the most
 - and least common elements in the result. What do you get if you take the
 - quantity of the most common element and subtract the quantity of the least
 - common element?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Data.List as L
import qualified Data.Maybe as Maybe
import qualified Data.InfMultiSet as MS
import qualified Data.Map as Map

main :: IO ()
main = defaultMain parseInput handleInput

data Token a = Token !a !a deriving (Show, Eq, Ord)
data Rule a = Rule
    { _token  :: !(Token a)
    , _middle :: a
    } deriving (Show, Eq, Ord)
type Rules a = [Rule a]

handleInput :: (String, Rules Char) -> IO ()
handleInput (seed, rules)
    = print
    . evaluate
    . MS.deleteMany (init . tail $ seed)
    . MS.unions
    . Maybe.catMaybes
    . map (flip Map.lookup table)
    . map (uncurry Token)
    . pairs
    $ seed
  where
    table = buildTable rules 40

evaluate :: MS.MultiSet a -> Integer
evaluate ms = mostFrequent - leastFrequent
  where
    frequencies = map snd . MS.toOccurList $ ms
    mostFrequent = maximum frequencies
    leastFrequent = minimum frequencies

pairs :: [a] -> [(a, a)]
pairs = Maybe.catMaybes . map f . L.tails
  where
    f (x1:x2:_) = Just (x1, x2)
    f _ = Nothing

buildTable :: Ord a => Rules a -> Int -> Map.Map (Token a) (MS.MultiSet a)
buildTable rules 0 =
    Map.fromList . map (\tok -> (tok, tokenSet tok)) . map _token $ rules
buildTable rules n
    = Map.fromList
    . map (\rule -> (_token rule, f rule))
    $ rules
  where
    table = buildTable rules (n - 1)
    f (Rule (Token x1 x2) x3) =
        let Just left = Map.lookup (Token x1 x3) table
            Just right = Map.lookup (Token x3 x2) table
        in MS.delete x3 $ MS.union left right

tokenSet :: Ord a => Token a -> MS.MultiSet a
tokenSet (Token a b) = MS.fromList [a, b]

parseInput :: T.Text -> Either P.ParseError (String, Rules Char)
parseInput = P.parse (parseProblem <* P.eof) ""

parseProblem :: P.Parsec T.Text () (String, Rules Char)
parseProblem = (,) <$> P.many1 P.alphaNum <* P.string "\n\n" <*> parseRules

parseRules :: P.Parsec T.Text () (Rules Char)
parseRules = parseRule `P.endBy` P.newline

parseRule :: P.Parsec T.Text () (Rule Char)
parseRule = Rule <$> parseToken <* P.string " -> " <*> P.alphaNum

parseToken :: P.Parsec T.Text () (Token Char)
parseToken = Token <$> P.alphaNum <*> P.alphaNum
