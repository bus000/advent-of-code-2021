{- --- Day 12: Passage Pathing ---
 -
 - With your submarine's subterranean subsystems subsisting suboptimally, the
 - only way you're getting out of this cave anytime soon is by finding a path
 - yourself. Not just a path - the only way to know if you've found the best
 - path is to find all of them.
 -
 - Fortunately, the sensors are still mostly working, and so you build a rough
 - map of the remaining caves (your puzzle input). For example:
 -
 -    start-A
 -    start-b
 -    A-c
 -    A-b
 -    b-d
 -    A-end
 -    b-end
 -
 - This is a list of how all of the caves are connected. You start in the cave
 - named start, and your destination is the cave named end. An entry like b-d
 - means that cave b is connected to cave d - that is, you can move between
 - them.
 -
 - So, the above cave system looks roughly like this:
 -
 -        start
 -        /   \
 -    c--A-----b--d
 -        \   /
 -         end
 -
 - Your goal is to find the number of distinct paths that start at start, end at
 - end, and don't visit small caves more than once. There are two types of
 - caves: big caves (written in uppercase, like A) and small caves (written in
 - lowercase, like b). It would be a waste of time to visit any small cave more
 - than once, but big caves are large enough that it might be worth visiting
 - them multiple times. So, all paths you find should visit small caves at most
 - once, and can visit big caves any number of times.
 -
 - Given these rules, there are 10 paths through this example cave system:
 -
 -    start,A,b,A,c,A,end
 -    start,A,b,A,end
 -    start,A,b,end
 -    start,A,c,A,b,A,end
 -    start,A,c,A,b,end
 -    start,A,c,A,end
 -    start,A,end
 -    start,b,A,c,A,end
 -    start,b,A,end
 -    start,b,end
 -
 - (Each line in the above list corresponds to a single path; the caves visited
 - by that path are listed in the order they are visited and separated by
 - commas.)
 -
 - Note that in this cave system, cave d is never visited by any path: to do so,
 - cave b would need to be visited twice (once on the way to cave d and a second
 - time when returning from cave d), and since cave b is small, this is not
 - allowed.
 -
 - Here is a slightly larger example:
 -
 -    dc-end
 -    HN-start
 -    start-kj
 -    dc-start
 -    dc-HN
 -    LN-dc
 -    HN-end
 -    kj-sa
 -    kj-HN
 -    kj-dc
 -
 - The 19 paths through it are as follows:
 -
 -    start,HN,dc,HN,end
 -    start,HN,dc,HN,kj,HN,end
 -    start,HN,dc,end
 -    start,HN,dc,kj,HN,end
 -    start,HN,end
 -    start,HN,kj,HN,dc,HN,end
 -    start,HN,kj,HN,dc,end
 -    start,HN,kj,HN,end
 -    start,HN,kj,dc,HN,end
 -    start,HN,kj,dc,end
 -    start,dc,HN,end
 -    start,dc,HN,kj,HN,end
 -    start,dc,end
 -    start,dc,kj,HN,end
 -    start,kj,HN,dc,HN,end
 -    start,kj,HN,dc,end
 -    start,kj,HN,end
 -    start,kj,dc,HN,end
 -    start,kj,dc,end
 -
 - Finally, this even larger example has 226 paths through it:
 -
 -    fs-end
 -    he-DX
 -    fs-he
 -    start-DX
 -    pj-DX
 -    end-zg
 -    zg-sl
 -    zg-pj
 -    pj-he
 -    RW-he
 -    fs-DX
 -    pj-RW
 -    zg-RW
 -    start-pj
 -    he-WI
 -    zg-he
 -    pj-fs
 -    start-RW
 -
 - How many paths through this cave system are there that visit small caves at
 - most once?
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
countPathsBetween start end system = go Set.empty start
  where
    go seen node
        | node == end = 1
        | node `Set.member` seen = 0
        | otherwise = case Map.lookup node system of
            Just (Cave _ neighbours True) ->
                sum . map (go seen) $ neighbours
            Just (Cave _ neighbours False) ->
                sum . map (go (Set.insert node seen)) $ neighbours
            Nothing -> 0

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
