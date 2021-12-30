{- You notice a second question on the back of the homework assignment:
 -
 - What is the largest magnitude you can get from adding only two of the
 - snailfish numbers?
 -
 - Note that snailfish addition is not commutative - that is, x + y and y + x
 - can produce different results.
 -
 - Again considering the last example homework assignment above:
 -
 -    [[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
 -    [[[5,[2,8]],4],[5,[[9,9],0]]]
 -    [6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
 -    [[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
 -    [[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
 -    [[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
 -    [[[[5,4],[7,7]],8],[[8,3],8]]
 -    [[9,3],[[9,9],[6,[4,9]]]]
 -    [[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
 -    [[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
 -
 - The largest magnitude of the sum of any two snailfish numbers in this list is
 - 3993. This is the magnitude of [[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]] +
 - [[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]], which reduces to
 - [[[[7,8],[6,6]],[[6,0],[7,7]]],[[[7,8],[8,8]],[[7,9],[0,6]]]].
 -
 - What is the largest magnitude of any sum of two different snailfish numbers
 - from the homework assignment?
 -}
{-# LANGUAGE DeriveFunctor #-}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Data.List as L
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import Control.Applicative ((<|>))
import qualified Control.Monad as M

data BinTree a = Node !(BinTree a) !(BinTree a) | Leaf !a
  deriving (Show, Eq, Ord, Functor)

data Direction = L | R deriving (Show, Eq, Ord)
type Context a = [(Direction, BinTree a)]

data Zipper a = Zipper
    { _element :: !(BinTree a)
    , _context :: !(Context a)
    } deriving (Show, Eq, Ord)

type SnailFishNumber = BinTree Int

main :: IO ()
main = defaultMain parseInput handleInput

handleInput :: [SnailFishNumber] -> IO ()
handleInput = print . largestMagnitude

largestMagnitude :: [SnailFishNumber] -> Int
largestMagnitude [] = 0
largestMagnitude sns = maximum $ do
    sn1 <- sns
    sn2 <- sns
    M.guard $ sn1 /= sn2
    return . magnitude $ snailAdd sn1 sn2

magnitude :: SnailFishNumber -> Int
magnitude (Leaf n) = n
magnitude (Node left right) = 3 * magnitude left + 2 * magnitude right

showSnail :: SnailFishNumber -> String
showSnail (Leaf n) = show n
showSnail (Node left right) =
    "[" ++ showSnail left ++ "," ++ showSnail right ++ "]"

snailSum :: [SnailFishNumber] -> SnailFishNumber
snailSum [] = Leaf 0
snailSum (n:ns) = L.foldl' snailAdd n ns

snailAdd :: SnailFishNumber -> SnailFishNumber -> SnailFishNumber
snailAdd s1 s2 = reduce $ Node s1 s2

reduce :: SnailFishNumber -> SnailFishNumber
reduce sn = case explode sn <|> split sn of
    Just sn' -> reduce sn'
    Nothing -> sn

explode :: SnailFishNumber -> Maybe SnailFishNumber
explode = fmap unZipper . go . newZipper
  where
    go :: Zipper Int -> Maybe (Zipper Int)
    go z@(Zipper (Node (Leaf n1) (Leaf n2)) (_:_:_:_:_)) =
        Just . addLeft n1 . addRight n2 . put (Leaf 0) $ z
    go (Zipper (Leaf _) _) = Nothing
    go z = go (goLeft z) <|> go (goRight z)

    addRight n z = case next z of
        Nothing -> z
        Just z' -> case previous (modify (+n) z') of
            Just z'' -> z''
            Nothing -> error "We were just at the previous one."

    addLeft n z = case previous z of
        Nothing -> z
        Just z' -> case next (modify (+n) z') of
            Just z'' -> z''
            Nothing -> error "We were just at the next one."

split :: SnailFishNumber -> Maybe SnailFishNumber
split = fmap unZipper . go . newZipper
  where
    go :: Zipper Int -> Maybe (Zipper Int)
    go z@(Zipper (Leaf n) _)
        | n >= 10 = Just $ put (Node (Leaf (down n)) (Leaf (up n))) z
        | otherwise = Nothing
    go z = go (goLeft z) <|> go (goRight z)

    down x = x `div` 2
    up x = abs $ (-x) `div` 2

newZipper :: BinTree a -> Zipper a
newZipper tree = Zipper tree []

goLeft :: Zipper a -> Zipper a
goLeft z@(Zipper (Leaf _) _) = z
goLeft (Zipper (Node left right) context) = Zipper left $ ((L, right):context)

goRight :: Zipper a -> Zipper a
goRight z@(Zipper (Leaf _) _) = z
goRight (Zipper (Node left right) context) = Zipper right $ ((R, left):context)

goUp :: Zipper a -> Zipper a
goUp (Zipper x []) = Zipper x []
goUp (Zipper left ((L, right):context)) = Zipper (Node left right) context
goUp (Zipper right ((R, left):context)) = Zipper (Node left right) context

toTop :: Zipper a -> Zipper a
toTop z@(Zipper _ []) = z
toTop z = toTop . goUp $ z

next :: Zipper a -> Maybe (Zipper a)
next zipper
    | isLeaf zipper = go zipper
    | otherwise = Nothing
  where
    go (Zipper _ []) = Nothing
    go z@(Zipper _ ((R, _):_)) = go . goUp $ z
    go z@(Zipper _ ((L, _):_)) = Just . leftMost . goRight . goUp $ z

    leftMost z
        | isLeaf z = z
        | otherwise = leftMost . goLeft $ z

previous :: Zipper a -> Maybe (Zipper a)
previous zipper
    | isLeaf zipper = go zipper
    | otherwise = Nothing
  where
    go (Zipper _ []) = Nothing
    go z@(Zipper _ ((L, _):_)) = go . goUp $ z
    go z@(Zipper _ ((R, _):_)) = Just . rightMost . goLeft . goUp $ z

    rightMost z
        | isLeaf z = z
        | otherwise = rightMost . goRight $ z

unZipper :: Zipper a -> BinTree a
unZipper = _element . toTop

isLeaf :: Zipper a -> Bool
isLeaf (Zipper (Node _ _) _) = False
isLeaf _ = True

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Zipper x context) = Zipper (f <$> x) context

put :: BinTree a -> Zipper a -> Zipper a
put x (Zipper _ context) = Zipper x context

parseInput :: T.Text -> Either P.ParseError [SnailFishNumber]
parseInput = P.parse (parseNumbers <* P.eof) ""

parseNumbers :: P.Parsec T.Text () [SnailFishNumber]
parseNumbers = parseNumber `P.endBy` P.newline

parseNumber :: P.Parsec T.Text () SnailFishNumber
parseNumber = P.choice [literal, composite]
  where
    literal = Leaf <$> P.int
    composite = P.between (P.char '[') (P.char ']')
        $ Node <$> parseNumber <* P.char ',' <*> parseNumber
