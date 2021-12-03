{- --- Day 2: Dive! ---
 -
 - Now, you need to figure out how to pilot this thing.
 -
 - It seems like the submarine can take a series of commands like forward 1,
 - down 2, or up 3:
 -
 - * forward X increases the horizontal position by X units.
 - * down X increases the depth by X units.
 - * up X decreases the depth by X units.
 -
 - Note that since you're on a submarine, down and up affect your depth, and so
 - they have the opposite result of what you might expect.
 -
 - The submarine seems to already have a planned course (your puzzle input). You
 - should probably figure out where it's going. For example:
 -
 -    forward 5
 -    down 5
 -    forward 8
 -    up 3
 -    down 8
 -    forward 2
 -
 - Your horizontal position and depth both start at 0. The steps above would
 - then modify them as follows:
 -
 - * forward 5 adds 5 to your horizontal position, a total of 5.
 - * down 5 adds 5 to your depth, resulting in a value of 5.
 - * forward 8 adds 8 to your horizontal position, a total of 13.
 - * up 3 decreases your depth by 3, resulting in a value of 2.
 - * down 8 adds 8 to your depth, resulting in a value of 10.
 - * forward 2 adds 2 to your horizontal position, a total of 15.
 -
 - After following these instructions, you would have a horizontal position of
 - 15 and a depth of 10. (Multiplying these together produces 150.)
 -
 - Calculate the horizontal position and depth you would have after following
 - the planned course. What do you get if you multiply your final horizontal
 - position by your final depth?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import Control.Applicative ((<|>))

main :: IO ()
main = defaultMain parseInput handleInput

data Command = Forward !Int | Down !Int | Up !Int deriving (Show, Eq)

data Position = Position !Int !Int

handleInput :: [Command] -> IO ()
handleInput steps = print . mulPos . foldr step startPosition $ steps
  where
    startPosition = Position 0 0

step :: Command -> Position -> Position
step (Forward n) (Position x y) = Position (x + n) y
step (Down n) (Position x y) = Position x (y + n)
step (Up n) (Position x y) = Position x (y - n)

mulPos :: Position -> Int
mulPos (Position x y) = x * y

parseInput :: T.Text -> Either P.ParseError [Command]
parseInput = P.parse (parseCommands <* P.eof) ""

parseCommands :: P.Parsec T.Text () [Command]
parseCommands = parseCommand `P.endBy` P.newline

parseCommand :: P.Parsec T.Text () Command
parseCommand = forward <|> down <|> up
  where
    forward = Forward <$> (P.string "forward " *> P.int)
    down = Down <$> (P.string "down " *> P.int)
    up = Up <$> (P.string "up " *> P.int)
