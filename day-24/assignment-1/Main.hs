{- --- Day 24: Arithmetic Logic Unit ---
 -
 - Magic smoke starts leaking from the submarine's arithmetic logic unit (ALU).
 - Without the ability to perform basic arithmetic and logic functions, the
 - submarine can't produce cool patterns with its Christmas lights!
 -
 - It also can't navigate. Or run the oxygen system.
 -
 - Don't worry, though - you probably have enough oxygen left to give you enough
 - time to build a new ALU.
 -
 - The ALU is a four-dimensional processing unit: it has integer variables w, x,
 - y, and z. These variables all start with the value 0. The ALU also supports
 - six instructions:
 -
 - * inp a - Read an input value and write it to variable a.
 - * add a b - Add the value of a to the value of b, then store the result in
 -   variable a.
 - * mul a b - Multiply the value of a by the value of b, then store the result
 -   in variable a.
 - * div a b - Divide the value of a by the value of b, truncate the result to
 -   an integer, then store the result in variable a. (Here, "truncate" means to
 -   round the value toward zero.)
 - * mod a b - Divide the value of a by the value of b, then store the remainder
 -   in variable a. (This is also called the modulo operation.)
 - * eql a b - If the value of a and b are equal, then store the value 1 in
 -   variable a. Otherwise, store the value 0 in variable a.
 -
 - In all of these instructions, a and b are placeholders; a will always be the
 - variable where the result of the operation is stored (one of w, x, y, or z),
 - while b can be either a variable or a number. Numbers can be positive or
 - negative, but will always be integers.
 -
 - The ALU has no jump instructions; in an ALU program, every instruction is run
 - exactly once in order from top to bottom. The program halts after the last
 - instruction has finished executing.
 -
 - (Program authors should be especially cautious; attempting to execute div
 - with b=0 or attempting to execute mod with a<0 or b<=0 will cause the program
 - to crash and might even damage the ALU. These operations are never intended
 - in any serious ALU program.)
 -
 - For example, here is an ALU program which takes an input number, negates it,
 - and stores it in x:
 -
 -    inp x
 -    mul x -1
 -
 - Here is an ALU program which takes two input numbers, then sets z to 1 if the
 - second input number is three times larger than the first input number, or
 - sets z to 0 otherwise:
 -
 -    inp z
 -    inp x
 -    mul z 3
 -    eql z x
 -
 - Here is an ALU program which takes a non-negative integer as input, converts
 - it into binary, and stores the lowest (1's) bit in z, the second-lowest (2's)
 - bit in y, the third-lowest (4's) bit in x, and the fourth-lowest (8's) bit in
 - w:
 -
 -    inp w
 -    add z w
 -    mod z 2
 -    div w 2
 -    add y w
 -    mod y 2
 -    div w 2
 -    add x w
 -    mod x 2
 -    div w 2
 -    mod w 2
 -
 - Once you have built a replacement ALU, you can install it in the submarine,
 - which will immediately resume what it was doing when the ALU failed:
 - validating the submarine's model number. To do this, the ALU will run the
 - MOdel Number Automatic Detector program (MONAD, your puzzle input).
 -
 - Submarine model numbers are always fourteen-digit numbers consisting only of
 - digits 1 through 9. The digit 0 cannot appear in a model number.
 -
 - When MONAD checks a hypothetical fourteen-digit model number, it uses
 - fourteen separate inp instructions, each expecting a single digit of the
 - model number in order of most to least significant. (So, to check the model
 - number 13579246899999, you would give 1 to the first inp instruction, 3 to
 - the second inp instruction, 5 to the third inp instruction, and so on.) This
 - means that when operating MONAD, each input instruction should only ever be
 - given an integer value of at least 1 and at most 9.
 -
 - Then, after MONAD has finished running all of its instructions, it will
 - indicate that the model number was valid by leaving a 0 in variable z.
 - However, if the model number was invalid, it will leave some other non-zero
 - value in z.
 -
 - MONAD imposes additional, mysterious restrictions on model numbers, and
 - legend says the last copy of the MONAD documentation was eaten by a tanuki.
 - You'll need to figure out what MONAD does some other way.
 -
 - To enable as many submarine features as possible, find the largest valid
 - fourteen-digit model number that contains no 0 digits. What is the largest
 - model number accepted by MONAD?
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
            M.firstJustM f [9, 8..1]
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
