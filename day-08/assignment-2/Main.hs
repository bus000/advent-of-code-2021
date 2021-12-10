{- Through a little deduction, you should now be able to determine the remaining
 - digits. Consider again the first example above:
 -
 -    acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
 -    cdfeb fcadb cdfeb cdbaf
 -
 - After some careful analysis, the mapping between signal wires and segments
 - only make sense in the following configuration:
 -
 -     dddd
 -    e    a
 -    e    a
 -     ffff
 -    g    b
 -    g    b
 -     cccc
 -
 - So, the unique signal patterns would correspond to the following digits:
 -
 -    acedgfb: 8
 -    cdfbe: 5
 -    gcdfa: 2
 -    fbcad: 3
 -    dab: 7
 -    cefabd: 9
 -    cdfgeb: 6
 -    eafb: 4
 -    cagedb: 0
 -    ab: 1
 -
 - Then, the four digits of the output value can be decoded:
 -
 -    cdfeb: 5
 -    fcadb: 3
 -    cdfeb: 5
 -    cdbaf: 3
 -
 - Therefore, the output value for this entry is 5353.
 -
 - Following this same process for each entry in the second, larger example
 - above, the output value of each entry can be determined:
 -
 -    fdgacbe cefdb cefbgd gcbe: 8394
 -    fcgedb cgb dgebacf gc: 9781
 -    cg cg fdcagb cbg: 1197
 -    efabcd cedba gadfec cb: 9361
 -    gecf egdcabf bgf bfgea: 4873
 -    gebdcfa ecba ca fadegcb: 8418
 -    cefg dcbef fcge gbcadfe: 4548
 -    ed bcgafe cdgba cbgef: 1625
 -    gbdfcae bgc cg cgb: 8717
 -    fgae cfgab fg bagce: 4315
 -
 - Adding all of the output values in this larger example produces 61229.
 -
 - For each entry, determine all of the wire/segment connections and decode the
 - four-digit output values. What do you get if you add up all of the output
 - values?
 -}
module Main where

import AdventOfCode
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Data.Maybe as Maybe
import qualified Data.List as L

main :: IO ()
main = defaultMain parseInput handleInput

data Signal a = Signal [a] deriving (Show, Ord)

instance Eq a => Eq (Signal a) where
    s1 == s2 = s1 `containsAll` s2 && s2 `containsAll` s1

data Example a = Example
    { _examples :: [Signal a]
    , _signal   :: [Signal a]
    } deriving (Show, Eq, Ord)

handleInput :: (Show a, Eq a) => [Example a] -> IO ()
handleInput = print . sum . Maybe.catMaybes . map exampleNumber

exampleNumber :: Eq a => Example a -> Maybe Int
exampleNumber example = do
    numbers <- listNumbers example
    indices <- mapM (`L.elemIndex` numbers) . _signal $ example
    return . read . concatMap show $ indices

listNumbers :: Eq a => Example a -> Maybe [Signal a]
listNumbers (Example examples _) = do
    one <- findOne examples
    four <- findFour examples
    seven <- findSeven examples
    eight <- findEight examples
    nine <- findNine examples four
    three <- findThree examples seven
    zero <- findZero examples one nine
    six <- findSix examples zero nine
    five <- findFive examples six
    two <- findTwo examples three five

    return [zero, one, two, three, four, five, six, seven, eight, nine]

-- Zero and nine are the only numbers with all of one and six segments.
findZero :: Eq a => [Signal a] -> Signal a -> Signal a -> Maybe (Signal a)
findZero examples one nine = Maybe.listToMaybe . filter isZero $ examples
  where
    isZero candidate = candidate `containsAll` one
        && signalLengthIs 6 candidate
        && candidate /= nine

-- One is the only number with two segments.
findOne :: [Signal a] -> Maybe (Signal a)
findOne = Maybe.listToMaybe . filter (signalLengthIs 2)

-- Two is the only 5 segment number besides five and three.
findTwo :: Eq a => [Signal a] -> Signal a -> Signal a -> Maybe (Signal a)
findTwo examples three five = Maybe.listToMaybe . filter isTwo $ examples
  where
    isTwo candidate = signalLengthIs 5 candidate
        && candidate /= three
        && candidate /= five

-- Three is the only number that contains all of seven and has 5 segments.
findThree :: Eq a => [Signal a] -> Signal a -> Maybe (Signal a)
findThree examples seven = Maybe.listToMaybe . filter isThree $ examples
  where
    isThree candidate = candidate `containsAll` seven
        && signalLengthIs 5 candidate

-- Four is the only number with 4 segments.
findFour :: [Signal a] -> Maybe (Signal a)
findFour = Maybe.listToMaybe . filter (signalLengthIs 4)

-- Five has 5 segments and is completely contained within 6.
findFive :: Eq a => [Signal a] -> Signal a -> Maybe (Signal a)
findFive examples six = Maybe.listToMaybe . filter isFive $ examples
  where
    isFive candidate = signalLengthIs 5 candidate
        && six `containsAll` candidate

-- Six, zero and nine are the only 6 segment numbers.
findSix :: Eq a => [Signal a] -> Signal a -> Signal a -> Maybe (Signal a)
findSix examples zero nine = Maybe.listToMaybe . filter isSix $ examples
  where
    isSix candidate = signalLengthIs 6 candidate
        && candidate /= zero
        && candidate /= nine

-- Seven is the only number with 3 segments.
findSeven :: [Signal a] -> Maybe (Signal a)
findSeven = Maybe.listToMaybe . filter (signalLengthIs 3)

-- Eight is the only number with 7 segments.
findEight :: [Signal a] -> Maybe (Signal a)
findEight = Maybe.listToMaybe . filter (signalLengthIs 7)

-- Nine is the only number that contains all of the same characters as four
-- plus 2 extra not in 4 four.
findNine :: Eq a => [Signal a] -> Signal a -> Maybe (Signal a)
findNine examples four = Maybe.listToMaybe . filter isNine $ examples
  where
    isNine candidate = candidate `containsAll` four
        && signalLengthIs 6 candidate

signalLength :: Signal a -> Int
signalLength (Signal s) = length s

signalLengthIs :: Int -> Signal a -> Bool
signalLengthIs l s = signalLength s == l

-- s1 contains all of s2?
containsAll :: Eq a => Signal a -> Signal a -> Bool
containsAll (Signal s1) (Signal s2) = all (`elem` s1) s2

parseInput :: T.Text -> Either P.ParseError [Example Char]
parseInput = P.parse (parseExamples <* P.eof) ""

parseExamples :: P.Parsec T.Text () [Example Char]
parseExamples = parseExample `P.endBy` P.newline

parseExample :: P.Parsec T.Text () (Example Char)
parseExample = Example <$> parseSignals 10 <* P.string " | " <*> parseSignals 4

parseSignals :: Int -> P.Parsec T.Text () [Signal Char]
parseSignals 0 = return []
parseSignals count = (:)
    <$> parseSignal
    <*> P.count (count - 1) (P.char ' ' *> parseSignal)

parseSignal :: P.Parsec T.Text () (Signal Char)
parseSignal = Signal <$> P.many1 P.alphaNum
