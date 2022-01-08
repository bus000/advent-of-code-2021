{- Now that you're warmed up, it's time to play the real game.
 -
 - A second compartment opens, this time labeled Dirac dice. Out of it falls a
 - single three-sided die.
 -
 - As you experiment with the die, you feel a little strange. An informational
 - brochure in the compartment explains that this is a quantum die: when you
 - roll it, the universe splits into multiple copies, one copy for each possible
 - outcome of the die. In this case, rolling the die always splits the universe
 - into three copies: one where the outcome of the roll was 1, one where it was
 - 2, and one where it was 3.
 -
 - The game is played the same as before, although to prevent things from
 - getting too far out of hand, the game now ends when either player's score
 - reaches at least 21.
 -
 - Using the same starting positions as in the example above, player 1 wins in
 - 444356092776315 universes, while player 2 merely wins in 341960390180808
 - universes.
 -
 - Using your given starting positions, determine every possible outcome. Find
 - the player that wins in more universes; in how many universes does that
 - player win?
 -}
{-# LANGUAGE LambdaCase #-}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Control.Monad.State.Strict as M

main :: IO ()
main = defaultMain parseInput handleInput

data Player = Player
    { _name     :: !Int
    , _position :: !Int
    , _score    :: !Int
    } deriving (Show, Eq, Ord)

data GameState = GameState
    { _player1   :: !Player -- Player 1 in the game.
    , _player2   :: !Player -- Player 2 in the game.
    } deriving (Show, Eq, Ord)

type QuantumGame a = M.State (Map.Map GameState (Map.Map Int Integer)) a

handleInput :: GameState -> IO ()
handleInput = print . maximum . Map.elems . winningWorlds

winningWorlds :: GameState -> Map.Map Int Integer
winningWorlds gs = M.evalState (movePlayer gs) Map.empty

movePlayer :: GameState -> QuantumGame (Map.Map Int Integer)
movePlayer gs@(GameState p1@(Player _ _ score1) p2@(Player _ _ score2))
    | score1 >= 21 = return $ Map.singleton (_name p1) 1
    | score2 >= 21 = return $ Map.singleton (_name p2) 1
    | otherwise = lookupState gs >>= \case
        Just seen -> return seen
        Nothing -> do
            subworlds <- mapM (movePlayer . updateState gs) $ rolls
            let winners = Map.unionsWith (+) subworlds
            M.modify $ Map.insert gs winners
            return winners
  where
    rolls = do
        d1 <- [1, 2, 3]
        d2 <- [1, 2, 3]
        d3 <- [1, 2, 3]
        return $ d1 + d2 + d3

lookupState :: GameState -> QuantumGame (Maybe (Map.Map Int Integer))
lookupState gs = Map.lookup gs <$> M.get

updateState :: GameState -> Int -> GameState
updateState (GameState p1 p2) roll = GameState p2 p1'
  where
    position' = ((_position p1 + roll - 1) `mod` 10) + 1
    score' = _score p1 + position'
    p1' = p1 { _position = position', _score = score' }

newPlayer :: Int -> Int -> Player
newPlayer name position = Player name position 0

parseInput :: T.Text -> Either P.ParseError GameState
parseInput = P.parse (parsePlayers <* P.eof) ""

parsePlayers :: P.Parsec T.Text () GameState
parsePlayers = GameState
    <$> parsePlayer <* P.newline
    <*> parsePlayer <* P.newline

parsePlayer :: P.Parsec T.Text () Player
parsePlayer = newPlayer
    <$> (P.string "Player " *> P.int)
    <*> (P.string " starting position: " *> P.int)
