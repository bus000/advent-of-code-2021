{- Sometimes, it's a good idea to appreciate just how big the ocean is. Using
 - the Manhattan distance, how far apart do the scanners get?
 -
 - In the above example, scanners 2 (1105,-1205,1229) and 3 (-92,-2380,-20) are
 - the largest Manhattan distance apart. In total, they are 1197 + 1175 + 1249 =
 - 3621 units apart.
 -
 - What is the largest Manhattan distance between any two scanners?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Matrix as M
import qualified Data.Maybe as Maybe
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import Control.Monad (guard)
import qualified System.Exit as System

main :: IO ()
main = defaultMain parseInput handleInput

data Point = Point
    { _x :: !Int
    , _y :: !Int
    , _z :: !Int
    } deriving (Show, Eq, Ord)

data Scanner = Scanner
    { _name   :: !Int
    , _points :: !(M.Matrix Int)
    } deriving (Show, Eq)

data Ocean = Ocean
    { _scanners :: !(Set.Set Point)
    , _beacons  :: !(Set.Set Point)
    } deriving (Show, Eq, Ord)

handleInput :: [Scanner] -> IO ()
handleInput scanners = case deriveOcean scanners of
    Just ocean -> print $ greatestScannerDist ocean
    Nothing -> System.die "Could not find correct combination."

greatestScannerDist :: Ocean -> Int
greatestScannerDist (Ocean scanners _)
    | Set.null scanners = 0
    | otherwise = maximum $ do
        p1@(Point x1 y1 z1) <- Set.toList scanners
        p2@(Point x2 y2 z2) <- Set.toList scanners
        guard $ p1 /= p2
        return $ abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

deriveOcean :: [Scanner] -> Maybe Ocean
deriveOcean [] = Nothing
deriveOcean (scanner:scanners) = go initialOcean scanners
  where
    initialOcean =
        Ocean (Set.singleton $ Point 0 0 0) (Set.fromList . unScanner $ scanner)

    go ocean [] = Just ocean
    go ocean ss = do
        (corresponding@(Scanner name _), relPos) <- findCorresponding ocean ss
        let ocean' = combine ocean corresponding relPos
            ss' = filter (not . hasName name) ss
        go ocean' ss'

findCorresponding :: Ocean -> [Scanner] -> Maybe (Scanner, Point)
findCorresponding (Ocean _ points) scanners = Maybe.listToMaybe $ do
    candidate <- scanners
    rotation <- orientations candidate
    (translation, translated) <- translations points rotation

    guard $ isCorresponding translated
    return (translated, translation)
  where
    isCorresponding
        = (> 11)
        . Set.size
        . Set.intersection points
        . Set.fromList
        . unScanner

newScanner :: Int -> [Point] -> Scanner
newScanner name points = Scanner name points'
  where
    points' = M.transpose . M.fromLists . map toList $ points
    toList (Point x y z) = [x, y, z]

unScanner :: Scanner -> [Point]
unScanner = map fromList . M.toLists . M.transpose . _points
  where
    fromList [x, y, z] = Point x y z
    fromList _ = error "Should not be possible."

combine :: Ocean -> Scanner -> Point -> Ocean
combine (Ocean scanners beacons) scanner relPos = Ocean scanners' beacons'
  where
    scanners' = Set.insert relPos scanners
    beacons' = Set.union beacons (Set.fromList . unScanner $ scanner)

hasName :: Int -> Scanner -> Bool
hasName n1 (Scanner n2 _) = n1 == n2

translations :: Set.Set Point -> Scanner -> [(Point, Scanner)]
translations s1 s2 = do
    Point x1 y1 z1 <- Set.toList s1
    Point x2 y2 z2 <- unScanner s2
    let diff = Point (x1 - x2) (y1 - y2) (z1 - z2)
    return $ (diff, translate s2 diff)

orientations :: Scanner -> [Scanner]
orientations (Scanner name points) = map (Scanner name . rotate) rotations
  where
    rotate rotation = rotation `M.multStd` points
    rotations = map M.fromLists
        [ [ [ 1,  0,  0]
          , [ 0,  1,  0]
          , [ 0,  0,  1]
          ]
        , [ [ 0,  0,  1]
          , [ 0,  1,  0]
          , [-1,  0,  0]
          ]
        , [ [-1,  0,  0]
          , [ 0,  1,  0]
          , [ 0,  0, -1]
          ]
        , [ [ 0,  0, -1]
          , [ 0,  1,  0]
          , [ 1,  0,  0]
          ]
        , [ [ 0, -1,  0]
          , [ 1,  0,  0]
          , [ 0,  0,  1]
          ]
        , [ [ 0,  0,  1]
          , [ 1,  0,  0]
          , [ 0,  1,  0]
          ]
        , [ [ 0,  1,  0]
          , [ 1,  0,  0]
          , [ 0,  0, -1]
          ]
        , [ [ 0,  0, -1]
          , [ 1,  0,  0]
          , [ 0, -1,  0]
          ]
        , [ [ 0,  1,  0]
          , [-1,  0,  0]
          , [ 0,  0,  1]
          ]
        , [ [ 0,  0,  1]
          , [-1,  0,  0]
          , [ 0, -1,  0]
          ]
        , [ [ 0, -1,  0]
          , [-1,  0,  0]
          , [ 0,  0, -1]
          ]
        , [ [ 0,  0, -1]
          , [-1,  0,  0]
          , [ 0,  1,  0]
          ]
        , [ [ 1,  0,  0]
          , [ 0,  0, -1]
          , [ 0,  1,  0]
          ]
        , [ [ 0,  1,  0]
          , [ 0,  0, -1]
          , [-1,  0,  0]
          ]
        , [ [-1,  0,  0]
          , [ 0,  0, -1]
          , [ 0, -1,  0]
          ]
        , [ [ 0, -1,  0]
          , [ 0,  0, -1]
          , [ 1,  0,  0]
          ]
        , [ [ 1,  0,  0]
          , [ 0, -1,  0]
          , [ 0,  0, -1]
          ]
        , [ [ 0,  0, -1]
          , [ 0, -1,  0]
          , [-1,  0,  0]
          ]
        , [ [-1,  0,  0]
          , [ 0, -1,  0]
          , [ 0,  0,  1]
          ]
        , [ [ 0,  0,  1]
          , [ 0, -1,  0]
          , [ 1,  0,  0]
          ]
        , [ [ 1,  0,  0]
          , [ 0,  0,  1]
          , [ 0, -1,  0]
          ]
        , [ [ 0, -1,  0]
          , [ 0,  0,  1]
          , [-1,  0,  0]
          ]
        , [ [-1,  0,  0]
          , [ 0,  0,  1]
          , [ 0,  1,  0]
          ]
        , [ [ 0,  1,  0]
          , [ 0,  0,  1]
          , [ 1,  0,  0]
          ]
        ]

translate :: Scanner -> Point -> Scanner
translate (Scanner name points) (Point x y z) = Scanner name $ M.mapPos f points
  where
    f (1, _) n = x + n
    f (2, _) n = y + n
    f (3, _) n = z + n
    f (_, _) _ = error "Scanners never have more than 3 rows."

parseInput :: T.Text -> Either P.ParseError [Scanner]
parseInput = P.parse (parseScanners <* P.eof) ""

parseScanners :: P.Parsec T.Text () [Scanner]
parseScanners = parseScanner `P.sepBy` P.newline

parseScanner :: P.Parsec T.Text () Scanner
parseScanner = newScanner
    <$> (P.string "--- scanner " *> P.int <* P.string " ---\n")
    <*> (parsePoint `P.endBy` P.newline)

parsePoint :: P.Parsec T.Text () Point
parsePoint = Point <$> P.int <* P.char ',' <*> P.int <* P.char ',' <*> P.int
