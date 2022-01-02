{- --- Day 21: Dirac Dice ---
 -
 - There's not much to do as you slowly descend to the bottom of the ocean. The
 - submarine computer challenges you to a nice game of Dirac Dice.
 -
 - This game consists of a single die, two pawns, and a game board with a
 - circular track containing ten spaces marked 1 through 10 clockwise. Each
 - player's starting space is chosen randomly (your puzzle input). Player 1 goes
 - first.
 -
 - Players take turns moving. On each player's turn, the player rolls the die
 - three times and adds up the results. Then, the player moves their pawn that
 - many times forward around the track (that is, moving clockwise on spaces in
 - order of increasing value, wrapping back around to 1 after 10). So, if a
 - player is on space 7 and they roll 2, 2, and 1, they would move forward 5
 - times, to spaces 8, 9, 10, 1, and finally stopping on 2.
 -
 - After each player moves, they increase their score by the value of the space
 - their pawn stopped on. Players' scores start at 0. So, if the first player
 - starts on space 7 and rolls a total of 5, they would stop on space 2 and add
 - 2 to their score (for a total score of 2). The game immediately ends as a win
 - for any player whose score reaches at least 1000.
 -
 - Since the first game is a practice game, the submarine opens a compartment
 - labeled deterministic dice and a 100-sided die falls out. This die always
 - rolls 1 first, then 2, then 3, and so on up to 100, after which it starts
 - over at 1 again. Play using this die.
 -
 - For example, given these starting positions:
 -
 -    Player 1 starting position: 4
 -    Player 2 starting position: 8
 -
 - This is how the game would go:
 -
 - * Player 1 rolls 1+2+3 and moves to space 10 for a total score of 10.
 - * Player 2 rolls 4+5+6 and moves to space 3 for a total score of 3.
 - * Player 1 rolls 7+8+9 and moves to space 4 for a total score of 14.
 - * Player 2 rolls 10+11+12 and moves to space 6 for a total score of 9.
 - * Player 1 rolls 13+14+15 and moves to space 6 for a total score of 20.
 - * Player 2 rolls 16+17+18 and moves to space 7 for a total score of 16.
 - * Player 1 rolls 19+20+21 and moves to space 6 for a total score of 26.
 - * Player 2 rolls 22+23+24 and moves to space 6 for a total score of 22.
 -
 - ...after many turns...
 -
 - * Player 2 rolls 82+83+84 and moves to space 6 for a total score of 742.
 - * Player 1 rolls 85+86+87 and moves to space 4 for a total score of 990.
 - * Player 2 rolls 88+89+90 and moves to space 3 for a total score of 745.
 - * Player 1 rolls 91+92+93 and moves to space 10 for a final score, 1000.
 -
 - Since player 1 has at least 1000 points, player 1 wins and the game ends. At
 - this point, the losing player had 745 points and the die had been rolled a
 - total of 993 times; 745 * 993 = 739785.
 -
 - Play a practice game using the deterministic 100-sided die. The moment either
 - player wins, what do you get if you multiply the score of the losing player
 - by the number of times the die was rolled during the game?
 -}
{-# LANGUAGE LambdaCase #-}
module Main where

import AdventOfCode
import qualified Data.Text as T
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

type Die = [Int]

data GameState = GameState
    { _players   :: ![Player] -- Players in the game.
    , _die       :: !Die -- Infinite list of die rolls.
    , _rolls     :: !Int -- Number of rolls in the game.
    , _hasWinner :: !Bool -- Whether or not game is finished.
    } deriving (Show, Eq, Ord)

type Game a = M.State GameState a

handleInput :: [Player] -> IO ()
handleInput = print . evaluateGame . simulate

evaluateGame :: GameState -> Int
evaluateGame (GameState players _ rolls _) = looserScore * rolls
  where
    looserScore :: Int
    looserScore = sum . filter (< 1000) . map _score $ players

simulate :: [Player] -> GameState
simulate players = M.execState step initialState
  where
    die = map (\x -> (x `mod` 100) + 1) $ [0..]
    initialState = GameState players die 0 False

step :: Game ()
step = do
    movePlayers
    finished <- isFinished
    M.when (not finished) step

movePlayers :: Game ()
movePlayers = do
    players <- M.gets _players
    players' <- mapM movePlayer players
    M.modify $ \x -> x { _players = players' }

movePlayer :: Player -> Game Player
movePlayer (Player name position score) = do
    finished <- isFinished
    if finished
        then return $ Player name position score
        else doUpdate
  where
    doUpdate = do
        roll <- rollDie
        let position' = position + roll
            position'' = if position' > 10 then position' - 10 else position'
            score' = score + position''
        M.when (score' >= 1000) $ M.modify $ \x -> x { _hasWinner = True }
        return $ Player name position'' score'

isFinished :: Game Bool
isFinished = M.gets _hasWinner

rollDie :: Game Int
rollDie = M.gets _die >>= \case
    (x1:x2:x3:xs) -> do
        M.modify $ \x -> x { _die = xs, _rolls = _rolls x + 3 }
        return $ (x1 + x2 + x3) `rem` 10
    _ -> error "Die should be infinite."

newPlayer :: Int -> Int -> Player
newPlayer name position = Player name position 0

parseInput :: T.Text -> Either P.ParseError [Player]
parseInput = P.parse (parsePlayers <* P.eof) ""

parsePlayers :: P.Parsec T.Text () [Player]
parsePlayers = parsePlayer `P.endBy` P.newline

parsePlayer :: P.Parsec T.Text () Player
parsePlayer = newPlayer
    <$> (P.string "Player " *> P.int)
    <*> (P.string " starting position: " *> P.int)
