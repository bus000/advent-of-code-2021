{- As the submarine starts booting up things like the Retro Encabulator, you
 - realize that maybe you don't need all these submarine features after all.
 -
 - What is the smallest model number accepted by MONAD?
 -}
{-# LANGUAGE LambdaCase #-}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Text.Parsec as P
import qualified Text.Parsec.Number as P
import qualified Control.Monad.State as M
import qualified Control.Monad.Misc as M
import qualified System.Exit as S

main :: IO ()
main = defaultMain parseInput handleInput

type Program = [Instruction]

data Register = W | X | Y | Z deriving (Show, Eq, Ord)

data Argument
    = Literal !Int
    | RegArg !Register
  deriving (Show, Eq, Ord)

data Instruction
    = Inp !Register
    | Add !Register !Argument
    | Mul !Register !Argument
    | Div !Register !Argument
    | Mod !Register !Argument
    | Eql !Register !Argument
  deriving (Show, Eq, Ord)

data ALUState = ALUState
    { _registerW :: !Int
    , _registerX :: !Int
    , _registerY :: !Int
    , _registerZ :: !Int
    , _writes    :: !Int
    } deriving (Show, Eq, Ord)

handleInput :: Program -> IO ()
handleInput program = case largestModelNumber program of
    Just result -> putStrLn result
    Nothing -> S.die "Could not find any model number."

largestModelNumber :: Program -> Maybe String
largestModelNumber program =
    case M.evalState (pathTo0Z program state) Set.empty of
        Just path -> Just $ concatMap show path
        Nothing -> Nothing
  where
    state = ALUState 0 0 0 0 0

type ALU a = M.State (Set.Set ALUState) a

pathTo0Z :: Program -> ALUState -> ALU (Maybe [Int])
pathTo0Z [] state
    | readRegister Z state == 0 = return (Just [])
    | otherwise = return Nothing
pathTo0Z (Inp a:program) state = do
    seen <- M.get
    if state `Set.member` seen
        then return Nothing
        else do
            M.modify $ Set.insert state
            M.firstJustM f [1..9]
  where
    f input = pathTo0Z program (writeRegister a input state) >>= \case
        Just model -> return $ Just (input:model)
        Nothing -> return Nothing
pathTo0Z (Add a b:program) state = pathTo0Z program $ binOp a b (+) state
pathTo0Z (Mul a b:program) state = pathTo0Z program $ binOp a b (*) state
pathTo0Z (Div a b:program) state = pathTo0Z program $ binOp a b div state
pathTo0Z (Mod a b:program) state = pathTo0Z program $ binOp a b mod state
pathTo0Z (Eql a b:program) state = pathTo0Z program $ binOp a b f state
  where
    f aVal bVal = if aVal == bVal then 1 else 0

binOp :: Register -> Argument -> (Int -> Int -> Int) -> ALUState -> ALUState
binOp a b f state = state'
  where
    aVal = readRegister a state
    bVal = readArgument b state
    state' = writeRegister a (f aVal bVal) state

readRegister :: Register -> ALUState -> Int
readRegister W = _registerW
readRegister X = _registerX
readRegister Y = _registerY
readRegister Z = _registerZ

readArgument :: Argument -> ALUState -> Int
readArgument (RegArg a) state = readRegister a state
readArgument (Literal x) _ = x

writeRegister :: Register -> Int -> ALUState -> ALUState
writeRegister W x state = state { _registerW = x, _writes = _writes state + 1 }
writeRegister X x state = state { _registerX = x, _writes = _writes state + 1 }
writeRegister Y x state = state { _registerY = x, _writes = _writes state + 1 }
writeRegister Z x state = state { _registerZ = x, _writes = _writes state + 1 }

parseInput :: T.Text -> Either P.ParseError Program
parseInput = P.parse (parseProgram <* P.eof) ""

parseProgram :: P.Parsec T.Text () Program
parseProgram = parseInstruction `P.endBy` P.newline

parseInstruction :: P.Parsec T.Text () Instruction
parseInstruction = P.choice [ pinput, padd, pmul, pdiv, pmod, peql ]
  where
    pinput = Inp <$> (P.string "inp " *> parseRegister)
    padd = pbinOp Add "add "
    pmul = P.try (pbinOp Mul "mul ")
    pdiv = pbinOp Div "div "
    pmod = P.try (pbinOp Mod "mod ")
    peql = pbinOp Eql "eql "
    pbinOp f str = f
        <$> (P.string str *> parseRegister)
        <*> (P.char ' ' *> parseArgument)

parseArgument :: P.Parsec T.Text () Argument
parseArgument = P.choice [RegArg <$> parseRegister, Literal <$> P.int]

parseRegister :: P.Parsec T.Text () Register
parseRegister = f <$> P.oneOf "wxyz"
  where
    f 'w' = W
    f 'x' = X
    f 'y' = Y
    f 'z' = Z
    f err = error $ "Unexpected argument " ++ show err
