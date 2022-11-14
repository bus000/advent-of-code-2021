{- --- Day 23: Amphipod ---
 -
 - A group of amphipods notice your fancy submarine and flag you down. "With
 - such an impressive shell," one amphipod says, "surely you can help us with a
 - question that has stumped our best scientists."
 -
 - They go on to explain that a group of timid, stubborn amphipods live in a
 - nearby burrow. Four types of amphipods live there: Amber (A), Bronze (B),
 - Copper (C), and Desert (D). They live in a burrow that consists of a hallway
 - and four side rooms. The side rooms are initially full of amphipods, and the
 - hallway is initially empty.
 -
 - They give you a diagram of the situation (your puzzle input), including
 - locations of each amphipod (A, B, C, or D, each of which is occupying an
 - otherwise open space), walls (#), and open space (.).
 -
 - For example:
 -
 -    #############
 -    #...........#
 -    ###B#C#B#D###
 -      #A#D#C#A#
 -      #########
 -
 - The amphipods would like a method to organize every amphipod into side rooms
 - so that each side room contains one type of amphipod and the types are sorted
 - A-D going left to right, like this:
 -
 -    #############
 -    #...........#
 -    ###A#B#C#D###
 -      #A#B#C#D#
 -      #########
 -
 - Amphipods can move up, down, left, or right so long as they are moving into
 - an unoccupied open space. Each type of amphipod requires a different amount
 - of energy to move one step: Amber amphipods require 1 energy per step, Bronze
 - amphipods require 10 energy, Copper amphipods require 100, and Desert ones
 - require 1000. The amphipods would like you to find a way to organize the
 - amphipods that requires the least total energy.
 -
 - However, because they are timid and stubborn, the amphipods have some extra
 - rules:
 -
 - * Amphipods will never stop on the space immediately outside any room. They
 -   can move into that space so long as they immediately continue moving.
 -   (Specifically, this refers to the four open spaces in the hallway that are
 -   directly above an amphipod starting position.)
 - * Amphipods will never move from the hallway into a room unless that room is
 -   their destination room and that room contains no amphipods which do not
 -   also have that room as their own destination. If an amphipod's starting
 -   room is not its destination room, it can stay in that room until it leaves
 -   the room. (For example, an Amber amphipod will not move from the hallway
 -   into the right three rooms, and will only move into the leftmost room if
 -   that room is empty or if it only contains other Amber amphipods.)
 - * Once an amphipod stops moving in the hallway, it will stay in that spot
 -   until it can move into a room. (That is, once any amphipod starts moving,
 -   any other amphipods currently in the hallway are locked in place and will
 -   not move again until they can move fully into a room.)
 - * In the above example, the amphipods can be organized using a minimum of
 -   12521 energy. One way to do this is shown below.
 -
 - Starting configuration:
 -
 -    #############
 -    #...........#
 -    ###B#C#B#D###
 -      #A#D#C#A#
 -      #########
 -
 - One Bronze amphipod moves into the hallway, taking 4 steps and using 40
 - energy:
 -
 -    #############
 -    #...B.......#
 -    ###B#C#.#D###
 -      #A#D#C#A#
 -      #########
 -
 - The only Copper amphipod not in its side room moves there, taking 4 steps and
 - using 400 energy:
 -
 -    #############
 -    #...B.......#
 -    ###B#.#C#D###
 -      #A#D#C#A#
 -      #########
 -
 - A Desert amphipod moves out of the way, taking 3 steps and using 3000 energy,
 - and then the Bronze amphipod takes its place, taking 3 steps and using 30
 - energy:
 -
 -    #############
 -    #.....D.....#
 -    ###B#.#C#D###
 -      #A#B#C#A#
 -      #########
 -
 - The leftmost Bronze amphipod moves to its room using 40 energy:
 -
 -    #############
 -    #.....D.....#
 -    ###.#B#C#D###
 -      #A#B#C#A#
 -      #########
 -
 - Both amphipods in the rightmost room move into the hallway, using 2003 energy
 - in total:
 -
 -    #############
 -    #.....D.D.A.#
 -    ###.#B#C#.###
 -      #A#B#C#.#
 -      #########
 -
 - Both Desert amphipods move into the rightmost room using 7000 energy:
 -
 -    #############
 -    #.........A.#
 -    ###.#B#C#D###
 -      #A#B#C#D#
 -      #########
 -
 - Finally, the last Amber amphipod moves into its room, using 8 energy:
 -
 -    #############
 -    #...........#
 -    ###A#B#C#D###
 -      #A#B#C#D#
 -      #########
 -
 - What is the least energy required to organize the amphipods?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Util as Util

data Amphipod = Amber | Bronze | Copper | Desert deriving (Show, Eq, Ord)

data HallwayPosition
    = L1 -- Leftmost position.
    | L2 -- Left towards middle.
    | M1 -- Left middle.
    | M2 -- Middle.
    | M3 -- Right middle.
    | R1 -- Right towards middle.
    | R2 -- Rightmost.
  deriving (Show, Eq, Ord)

data RoomPosition
    = AmberTop
    | AmberBottom
    | BronzeTop
    | BronzeBottom
    | CopperTop
    | CopperBottom
    | DesertTop
    | DesertBottom
  deriving (Show, Eq, Ord)

data Position = Hallway !HallwayPosition | Room !RoomPosition
  deriving (Show, Eq, Ord)

data Burrow = Burrow
    { _pods :: !(Map.Map Position Amphipod)
    } deriving (Show, Eq, Ord)

-- The price of movement.
type Price = Word

-- A path in the burrow.
data Path = Path
    { _start  :: !Position
    , _middle :: ![Position]
    , _end    :: !Position
    } deriving (Show, Eq, Ord)

main :: IO ()
main = defaultMain parseInput handleInput

handleInput :: Burrow -> IO ()
handleInput = print . findSolution

findSolution :: Burrow -> Maybe (Burrow, Price)
findSolution = Maybe.listToMaybe . filter (isDone . fst) . reachableStates
--findSolution :: Burrow -> [(Burrow, Price)]
--findSolution = takeWhile (not . isDone . fst) . reachableStates
  where
    isDone = (==) done
    done = Burrow . Map.fromList $
        [ (Room AmberTop, Amber)
        , (Room AmberBottom, Amber)
        , (Room BronzeTop, Bronze)
        , (Room BronzeBottom, Bronze)
        , (Room CopperTop, Copper)
        , (Room CopperBottom, Copper)
        , (Room DesertTop, Desert)
        , (Room DesertBottom, Desert)
        ]

reachableStates :: Burrow -> [(Burrow, Price)]
reachableStates burrow = Util.dijkstra burrow move

-- Compute all possible moves from the burrow given with the price of that
-- movement.
move :: Burrow -> [(Burrow, Price)]
move burrow
    = map collapseEither
    . map (movePod burrow)
    . moves
    $ burrow
  where
    collapseEither (Left err) = error "Should not be able to happen."
    collapseEither (Right result) = result

movePod :: Burrow -> Path -> Either String (Burrow, Price)
movePod (Burrow pods) path@(Path start _ end) = case Map.lookup start pods of
    Just pod ->
        let inserted = Map.insert end pod pods
            removed = Map.delete start inserted
        in Right (Burrow removed, pathPrice path pod)
    Nothing -> Left $ "Pod could not be found at position " <> show start <> "."

-- Find all moves possible in the current burrow.
moves :: Burrow -> [Path]
moves burrow
    = filter pathFree
    . map (\(start, end) -> path start end)
    . concatMap (\(pos, pod) -> map (\t -> (pos, t)) (targets burrow pos pod))
    . Map.toList
    $ tiles
  where
    tiles = _pods burrow
    pathFree (Path _ middle end) =
        not $ any (flip Map.member $ tiles) (end:middle)

targets :: Burrow -> Position -> Amphipod -> [Position]
targets burrow (Hallway pos) Amber = case amberRoom burrow of
    (Nothing, Nothing) -> [Room AmberBottom]
    (Nothing, Just Amber) -> [Room AmberTop]
    _ -> []
targets burrow (Hallway pos) Bronze = case bronzeRoom burrow of
    (Nothing, Nothing) -> [Room BronzeBottom]
    (Nothing, Just Bronze) -> [Room BronzeTop]
    _ -> []
targets burrow (Hallway pos) Copper = case copperRoom burrow of
    (Nothing, Nothing) -> [Room CopperBottom]
    (Nothing, Just Copper) -> [Room CopperTop]
    _ -> []
targets burrow (Hallway pos) Desert = case desertRoom burrow of
    (Nothing, Nothing) -> [Room DesertBottom]
    (Nothing, Just Desert) -> [Room DesertTop]
    _ -> []
targets burrow (Room AmberBottom) Amber = []
targets burrow (Room AmberTop) Amber =
    case lookupOccupant burrow (Room AmberBottom) of
        Just Amber -> []
        _ -> hallway
targets burrow (Room BronzeBottom) Bronze = []
targets burrow (Room BronzeTop) Bronze =
    case lookupOccupant burrow (Room BronzeBottom) of
        Just Bronze -> []
        _ -> hallway
targets burrow (Room CopperBottom) Copper = []
targets burrow (Room CopperTop) Copper =
    case lookupOccupant burrow (Room CopperBottom) of
        Just Copper -> []
        _ -> hallway
targets burrow (Room DesertBottom) Desert = []
targets burrow (Room DesertTop) Desert =
    case lookupOccupant burrow (Room DesertBottom) of
        Just Desert -> []
        _ -> hallway
targets _ (Room _) _ = hallway

amberRoom :: Burrow -> (Maybe Amphipod, Maybe Amphipod)
amberRoom burrow =
    ( lookupOccupant burrow (Room AmberTop)
    , lookupOccupant burrow (Room AmberBottom)
    )

bronzeRoom :: Burrow -> (Maybe Amphipod, Maybe Amphipod)
bronzeRoom burrow =
    ( lookupOccupant burrow (Room BronzeTop)
    , lookupOccupant burrow (Room BronzeBottom)
    )

copperRoom :: Burrow -> (Maybe Amphipod, Maybe Amphipod)
copperRoom burrow =
    ( lookupOccupant burrow (Room CopperTop)
    , lookupOccupant burrow (Room CopperBottom)
    )

desertRoom :: Burrow -> (Maybe Amphipod, Maybe Amphipod)
desertRoom burrow =
    ( lookupOccupant burrow (Room DesertTop)
    , lookupOccupant burrow (Room DesertBottom)
    )

lookupOccupant :: Burrow -> Position -> Maybe Amphipod
lookupOccupant burrow pos = Map.lookup pos (_pods burrow)

hallway :: [Position]
hallway =
    [ Hallway L1
    , Hallway L2
    , Hallway M1
    , Hallway M2
    , Hallway M3
    , Hallway R1
    , Hallway R2
    ]

path :: Position -> Position -> Path
path (Hallway pos1) (Room pos2) = pathHome pos1 pos2
path (Room pos1) (Hallway pos2) = pathOut pos1 pos2
path _ _ = error "Cannot route from hallway -> hallway or room -> room."

-- Find shortest path from hallway to a room.
pathHome :: HallwayPosition -> RoomPosition -> Path
pathHome L1 end = prependPath (Hallway L1) (pathHome L2 end)

pathHome L2 AmberTop = Path (Hallway L2) [] (Room AmberTop)
pathHome L2 AmberBottom = Path (Hallway L2) [Room AmberTop] (Room AmberBottom)
pathHome L2 end = prependPath (Hallway L2) (pathHome M1 end)

pathHome M1 AmberTop = Path (Hallway M1) [] (Room AmberTop)
pathHome M1 AmberBottom = Path (Hallway M1) [Room AmberTop] (Room AmberBottom)
pathHome M1 BronzeTop = Path (Hallway M1) [] (Room BronzeTop)
pathHome M1 BronzeBottom = Path (Hallway M1) [Room BronzeTop] (Room BronzeBottom)
pathHome M1 end = prependPath (Hallway M1) (pathHome M2 end)

pathHome M2 BronzeTop = Path (Hallway M2) [] (Room BronzeTop)
pathHome M2 BronzeBottom = Path (Hallway M2) [Room BronzeTop] (Room BronzeBottom)
pathHome M2 CopperTop = Path (Hallway M2) [] (Room CopperTop)
pathHome M2 CopperBottom = Path (Hallway M2) [Room CopperTop] (Room CopperBottom)
pathHome M2 AmberTop = prependPath (Hallway M2) (pathHome M1 AmberTop)
pathHome M2 AmberBottom = prependPath (Hallway M2) (pathHome M1 AmberBottom)
pathHome M2 DesertTop = prependPath (Hallway M2) (pathHome M3 DesertTop)
pathHome M2 DesertBottom = prependPath (Hallway M2) (pathHome M3 DesertBottom)

pathHome M3 CopperTop = Path (Hallway M3) [] (Room CopperTop)
pathHome M3 CopperBottom = Path (Hallway M3) [Room CopperTop] (Room CopperBottom)
pathHome M3 DesertTop = Path (Hallway M3) [] (Room DesertTop)
pathHome M3 DesertBottom = Path (Hallway M3) [Room DesertTop] (Room DesertBottom)
pathHome M3 end = prependPath (Hallway M3) (pathHome M2 end)

pathHome R1 DesertTop = Path (Hallway R1) [] (Room DesertTop)
pathHome R1 DesertBottom = Path (Hallway R1) [Room DesertTop] (Room DesertBottom)
pathHome R1 end = prependPath (Hallway R1) (pathHome M3 end)

pathHome R2 end = prependPath (Hallway R2) (pathHome R1 end)

pathOut :: RoomPosition -> HallwayPosition -> Path
pathOut room hallway = reversePath $ pathHome hallway room

prependPath :: Position -> Path -> Path
prependPath position (Path start middle end) = Path position (start:middle) end

reversePath :: Path -> Path
reversePath (Path start middle end) = Path end (reverse middle) start

pathPrice :: Path -> Amphipod -> Price
pathPrice path pod = pathUnits path * pricePerUnit pod

pathUnits :: Path -> Word
pathUnits (Path start middle end) = sum . map pairUnits $ pairs
  where
    listPath = start : middle ++ [end]
    pairs = zip listPath (tail listPath)

    pairUnits (Hallway _, Room _) = 2
    pairUnits (Room _, Hallway _) = 2
    pairUnits _ = 1

pricePerUnit :: Amphipod -> Price
pricePerUnit Amber = 1
pricePerUnit Bronze = 10
pricePerUnit Copper = 100
pricePerUnit Desert = 1000

parseInput :: T.Text -> Either P.ParseError Burrow
parseInput text = return $ Burrow pods
  where
    a1 = (Room AmberTop, Bronze)
    a2 = (Room AmberBottom, Amber)
    b1 = (Room BronzeTop, Copper)
    b2 = (Room BronzeBottom, Desert)
    c1 = (Room CopperTop, Bronze)
    c2 = (Room CopperBottom, Copper)
    d1 = (Room DesertTop, Desert)
    d2 = (Room DesertBottom, Amber)
    pods = Map.fromList [a1, a2, b1, b2, c1, c2, d1, d2]

--    #############
--    #...........#
--    ###B#C#B#D###
--      #A#D#C#A#
--      #########

--data Burrow = Burrow
    --{ _pods :: !(Map.Map Position Amphipod)
    --} deriving (Show, Eq, Ord)

















-- parseInput :: T.Text -> Either P.ParseError Burrow
-- parseInput = P.parse (parseBurrow <* P.eof) ""
-- 
-- parseBurrow :: P.Parsec T.Text () Burrow
-- parseBurrow = do
--     P.count 2 skipLine
--     P.string "###"
--     a1 <- parseAmphipod
--     P.char '#'
--     a2 <- parseAmphipod
--     P.char '#'
--     a3 <- parseAmphipod
--     P.char '#'
--     a4 <- parseAmphipod
--     P.string "###\n"
--     P.string "  #"
--     a5 <- parseAmphipod
--     P.char '#'
--     a6 <- parseAmphipod
--     P.char '#'
--     a7 <- parseAmphipod
--     P.char '#'
--     a8 <- parseAmphipod
--     P.string "#\n"
--     skipLine
-- 
--     return $ Burrow
--         [ Amphipod a1 (Position Room1 0)
--         , Amphipod a2 (Position Room2 0)
--         , Amphipod a3 (Position Room3 0)
--         , Amphipod a4 (Position Room4 0)
--         , Amphipod a5 (Position Room1 1)
--         , Amphipod a6 (Position Room2 1)
--         , Amphipod a7 (Position Room3 1)
--         , Amphipod a8 (Position Room4 1)
--         ]
--   where
--     skipLine = P.many (P.oneOf ".# ") >> P.newline
-- 
-- parseAmphipod :: P.Parsec T.Text () AmphipodType
-- parseAmphipod = f <$> P.oneOf "ABCD"
--   where
--     f 'A' = Amber
--     f 'B' = Bronze
--     f 'C' = Copper
--     f 'D' = Desert
--     f err = error $ "Unexpected argument " ++ show err
