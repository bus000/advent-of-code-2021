{- Now that you have the structure of your transmission decoded, you can
 - calculate the value of the expression it represents.
 -
 - Literal values (type ID 4) represent a single number as described above. The
 - remaining type IDs are more interesting:
 -
 - * Packets with type ID 0 are sum packets - their value is the sum of the
 -   values of their sub-packets. If they only have a single sub-packet, their
 -   value is the value of the sub-packet.
 - * Packets with type ID 1 are product packets - their value is the result of
 -   multiplying together the values of their sub-packets. If they only have a
 -   single sub-packet, their value is the value of the sub-packet.
 - * Packets with type ID 2 are minimum packets - their value is the minimum of
 -   the values of their sub-packets.
 - * Packets with type ID 3 are maximum packets - their value is the maximum of
 -   the values of their sub-packets.
 - * Packets with type ID 5 are greater than packets - their value is 1 if the
 -   value of the first sub-packet is greater than the value of the second
 -   sub-packet; otherwise, their value is 0. These packets always have exactly
 -   two sub-packets.
 - * Packets with type ID 6 are less than packets - their value is 1 if the
 -   value of the first sub-packet is less than the value of the second
 -   sub-packet; otherwise, their value is 0. These packets always have exactly
 -   two sub-packets.
 - * Packets with type ID 7 are equal to packets - their value is 1 if the value
 -   of the first sub-packet is equal to the value of the second sub-packet;
 -   otherwise, their value is 0. These packets always have exactly two
 -   sub-packets.
 -
 - Using these rules, you can now work out the value of the outermost packet in
 - your BITS transmission.
 -
 - For example:
 -
 - * C200B40A82 finds the sum of 1 and 2, resulting in the value 3.
 - * 04005AC33890 finds the product of 6 and 9, resulting in the value 54.
 - * 880086C3E88112 finds the minimum of 7, 8, and 9, resulting in the value 7.
 - * CE00C43D881120 finds the maximum of 7, 8, and 9, resulting in the value 9.
 - * D8005AC2A8F0 produces 1, because 5 is less than 15.
 - * F600BC2D8F produces 0, because 5 is not greater than 15.
 - * 9C005AC2F8F0 produces 0, because 5 is not equal to 15.
 - * 9C0141080250320F1802104A08 produces 1, because 1 + 3 = 2 * 2.
 -
 - What do you get if you evaluate the expression represented by your
 - hexadecimal-encoded BITS transmission?
 -}
module Main where

import AdventOfCode
import GHC.Word (Word8, Word64)
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Data.ByteString as B
import qualified Data.Bits as Bits
import qualified Data.Binary.Strict.BitGet as BG
import qualified System.Exit as S

main :: IO ()
main = defaultMain parseInput handleInput

data Packet
    = LiteralPacket
        { _header  :: !PacketHeader
        , _literal :: !Word64
        }
    | SumPacket
        { _header     :: !PacketHeader
        , _subPackets :: ![Packet]
        }
    | ProductPacket
        { _header     :: !PacketHeader
        , _subPackets :: ![Packet]
        }
    | MinPacket
        { _header     :: !PacketHeader
        , _subPackets :: ![Packet]
        }
    | MaxPacket
        { _header     :: !PacketHeader
        , _subPackets :: ![Packet]
        }
    | GreaterThanPacket
        { _header :: !PacketHeader
        , _first  :: !Packet
        , _second :: !Packet
        }
    | LessThanPacket
        { _header :: !PacketHeader
        , _first  :: !Packet
        , _second :: !Packet
        }
    | EqualPacket 
        { _header :: !PacketHeader
        , _first  :: !Packet
        , _second :: !Packet
        }
    deriving (Show, Eq, Ord)

data PacketHeader = PacketHeader
    { _version    :: !Word8
    , _packetType :: !Word8
    } deriving (Show, Eq, Ord)

handleInput :: B.ByteString -> IO ()
handleInput input = case parseBITS input of
    Right packets -> print . evaluate $ packets
    Left err -> S.die err

evaluate :: Packet -> Word64
evaluate (LiteralPacket _ literal) = literal
evaluate (SumPacket _ subPackets) = sum . map evaluate $ subPackets
evaluate (ProductPacket _ subPackets) = product . map evaluate $ subPackets
evaluate (MinPacket _ subPackets) = minimum . map evaluate $ subPackets
evaluate (MaxPacket _ subPackets) = maximum . map evaluate $ subPackets
evaluate (GreaterThanPacket _ first second)
    | evaluate first > evaluate second = 1
    | otherwise = 0
evaluate (LessThanPacket _ first second)
    | evaluate first < evaluate second = 1
    | otherwise = 0
evaluate (EqualPacket _ first second)
    | evaluate first == evaluate second = 1
    | otherwise = 0

parseBITS :: B.ByteString -> Either String Packet
parseBITS str = BG.runBitGet str $ do
    packet <- parsePacket
    alignWholeByte
    finished <- BG.isEmpty
    if finished
        then return packet
        else fail "Has more bytes left."

parsePacket :: BG.BitGet Packet
parsePacket = do
    header@(PacketHeader _ packetType) <- parseHeader
    case packetType of
        0 -> subList $ SumPacket header
        1 -> subList $ ProductPacket header
        2 -> subList $ MinPacket header
        3 -> subList $ MaxPacket header
        4 -> literal $ LiteralPacket header
        5 -> binOp $ GreaterThanPacket header
        6 -> binOp $ LessThanPacket header
        7 -> binOp $ EqualPacket header
        n -> fail $ "Unexpected package type " ++ show n
  where
    subList f = f <$> parseSubPackets
    literal f = f <$> parseLiteral
    binOp f = do
        subPackets <- parseSubPackets
        case subPackets of
            [first, second] -> return $ f first second
            packets -> fail $ "Expected 2 subpackets, got " ++ show packets

parseHeader :: BG.BitGet PacketHeader
parseHeader = do
    version <- BG.getAsWord8 3
    packetType <- BG.getAsWord8 3

    return $ PacketHeader version packetType

parseLiteral :: BG.BitGet Word64
parseLiteral = foldl (\accum n -> accum `Bits.shift` 4 + n) 0 <$> go
  where
    go :: BG.BitGet [Word64]
    go = do
        isLast <- not <$> BG.getBit
        byte <- BG.getAsWord64 4
        if isLast
            then return [byte]
            else do
                rest <- go
                return $ byte : rest

parseSubPackets :: BG.BitGet [Packet]
parseSubPackets = do
    lengthType <- BG.getBit
    if lengthType
        then do
            subPacketN <- BG.getAsWord16 11
            parseNPackets subPacketN
        else do
            subPacketSize <- BG.getAsWord16 15
            bitsLeft <- BG.remaining
            packetsUntilBitsLeft (bitsLeft - fromIntegral subPacketSize)

parseNPackets :: Integral a => a -> BG.BitGet [Packet]
parseNPackets 0 = return []
parseNPackets n = do
    packet <- parsePacket
    rest <- parseNPackets (n - 1)
    return $ packet : rest

packetsUntilBitsLeft :: Int -> BG.BitGet [Packet]
packetsUntilBitsLeft n = do
    bitsLeft <- BG.remaining
    if bitsLeft <= n
        then return []
        else do
            packet <- parsePacket
            rest <- packetsUntilBitsLeft n
            return $ packet : rest

alignWholeByte :: BG.BitGet ()
alignWholeByte = do
    remaining <- BG.remaining
    BG.skip (remaining `mod` 8)

parseInput :: T.Text -> Either P.ParseError B.ByteString
parseInput = P.parse (parseHexString <* P.eof) ""

parseHexString :: P.Parsec T.Text () B.ByteString
parseHexString = B.pack <$> P.many1 parseHexByte <* P.newline

parseHexByte :: P.Parsec T.Text () Word8
parseHexByte = (+) <$> upper <*> lower
  where
    upper = (\x -> Bits.shift x 4) . f <$> P.hexDigit
    lower = f <$> P.hexDigit

    f '0' = 0x00
    f '1' = 0x01
    f '2' = 0x02
    f '3' = 0x03
    f '4' = 0x04
    f '5' = 0x05
    f '6' = 0x06
    f '7' = 0x07
    f '8' = 0x08
    f '9' = 0x09
    f 'A' = 0x0a
    f 'B' = 0x0b
    f 'C' = 0x0c
    f 'D' = 0x0d
    f 'E' = 0x0e
    f 'F' = 0x0f
    f err = error $ "Function called with unexpected argument " ++ show err
