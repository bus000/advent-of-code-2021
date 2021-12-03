module AdventOfCode
    ( defaultMain
    ) where

import qualified System.Exit as Sys
import qualified Data.Text as T
import qualified Data.Text.IO as T

{- | Default main method that can be used to solve the advent of code
 - assignments. The main method will handle reading input and exiting on parser
 - errors. -}
defaultMain
    :: Show e
    => (T.Text -> Either e a)
    -- ^ The input parser.
    -> (a -> IO ())
    -- ^ The action to perform on parsed input.
    -> IO ()
defaultMain parseInput handleInput = do
    parsedInput <- parseInput <$> T.getContents

    case parsedInput of
        Left err -> Sys.die $ show err
        Right input -> handleInput input
