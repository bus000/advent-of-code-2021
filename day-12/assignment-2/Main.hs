{- After reviewing the available paths, you realize you might have time to visit
 - a single small cave twice. Specifically, big caves can be visited any number
 - of times, a single small cave can be visited at most twice, and the remaining
 - small caves can be visited at most once. However, the caves named start and
 - end can only be visited exactly once each: once you leave the start cave, you
 - may not return to it, and once you reach the end cave, the path must end
 - immediately.
 -
 - Now, the 36 possible paths through the first example above are:
 -
 -    start,A,b,A,b,A,c,A,end
 -    start,A,b,A,b,A,end
 -    start,A,b,A,b,end
 -    start,A,b,A,c,A,b,A,end
 -    start,A,b,A,c,A,b,end
 -    start,A,b,A,c,A,c,A,end
 -    start,A,b,A,c,A,end
 -    start,A,b,A,end
 -    start,A,b,d,b,A,c,A,end
 -    start,A,b,d,b,A,end
 -    start,A,b,d,b,end
 -    start,A,b,end
 -    start,A,c,A,b,A,b,A,end
 -    start,A,c,A,b,A,b,end
 -    start,A,c,A,b,A,c,A,end
 -    start,A,c,A,b,A,end
 -    start,A,c,A,b,d,b,A,end
 -    start,A,c,A,b,d,b,end
 -    start,A,c,A,b,end
 -    start,A,c,A,c,A,b,A,end
 -    start,A,c,A,c,A,b,end
 -    start,A,c,A,c,A,end
 -    start,A,c,A,end
 -    start,A,end
 -    start,b,A,b,A,c,A,end
 -    start,b,A,b,A,end
 -    start,b,A,b,end
 -    start,b,A,c,A,b,A,end
 -    start,b,A,c,A,b,end
 -    start,b,A,c,A,c,A,end
 -    start,b,A,c,A,end
 -    start,b,A,end
 -    start,b,d,b,A,c,A,end
 -    start,b,d,b,A,end
 -    start,b,d,b,end
 -    start,b,end
 -
 - The slightly larger example above now has 103 paths through it, and the even
 - larger example now has 3509 paths through it.
 -
 - Given these new rules, how many paths through this cave system are there?
 -}
 {-# LANGUAGE OverloadedStrings #-}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

main :: IO ()
main = defaultMain parseInput handleInput

data Cave = Cave
    { _name       :: !T.Text
    , _neighbours :: ![T.Text]
    , _big        :: !Bool
    } deriving (Show, Eq, Ord)

type CaveSystem = Map.Map T.Text Cave
type Path = (T.Text, T.Text)

handleInput :: CaveSystem -> IO ()
handleInput = print . countPathsBetween "start" "end"

countPathsBetween :: T.Text -> T.Text -> CaveSystem -> Int
countPathsBetween start end system = go Set.empty False start
  where
    system' = removeTransitionsTo start system

    go seen collided node
        | node == end = 1
        | node `Set.member` seen && collided = 0
        | otherwise = case Map.lookup node system' of
            Nothing -> 0
            Just (Cave _ neighbours True) ->
                sum . map (go seen collided) $ neighbours
            Just (Cave _ neighbours False) ->
                sum . map (go (Set.insert node seen)
                    (collided || node `Set.member` seen)) $ neighbours

removeTransitionsTo :: T.Text -> CaveSystem -> CaveSystem
removeTransitionsTo identifier = Map.map (removeTransition identifier)

removeTransition :: T.Text -> Cave -> Cave
removeTransition identifier cave = cave
    { _neighbours = filter (/= identifier) . _neighbours $ cave
    }

buildCaveSystem :: [Path] -> CaveSystem 
buildCaveSystem = L.foldl' insertPath Map.empty

insertPath :: CaveSystem -> Path -> CaveSystem
insertPath system (c1, c2) = withSecond
  where
    withFirst = Map.insertWith mergeCaves c1 (buildCave c1 [c2]) system
    withSecond = Map.insertWith mergeCaves c2 (buildCave c2 [c1]) withFirst

mergeCaves :: Cave -> Cave -> Cave
mergeCaves c1 c2
    | _name c1 /= _name c2 = error "Cannot combine different caves."
    | otherwise = Cave
        { _name = _name c1
        , _neighbours = _neighbours c1 ++ _neighbours c2
        , _big = _big c1
        }

buildCave :: T.Text -> [T.Text] -> Cave
buildCave identifier neighbours = Cave
    { _name = identifier
    , _neighbours = neighbours
    , _big = not $ T.toLower identifier == identifier
    }

parseInput :: T.Text -> Either P.ParseError CaveSystem
parseInput = P.parse (parseCaveSystem <* P.eof) ""

parseCaveSystem :: P.Parsec T.Text () CaveSystem
parseCaveSystem = buildCaveSystem <$> parsePaths

parsePaths :: P.Parsec T.Text () [Path]
parsePaths = parsePath `P.endBy` P.newline

parsePath :: P.Parsec T.Text () Path
parsePath = (,) <$> parseIdentifier <* P.char '-' <*> parseIdentifier

parseIdentifier :: P.Parsec T.Text () T.Text
parseIdentifier = T.pack <$> P.many1 P.alphaNum
