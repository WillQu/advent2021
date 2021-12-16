module Day16 where

import Numeric
import Data.Char (intToDigit, digitToInt)
import Text.Printf

import Text.ParserCombinators.ReadP

data PacketValue = Literal Int | Operator Int [Packet] deriving Show

data Packet = Packet {
    version :: Int,
    value :: PacketValue
} deriving Show

sumVersions :: Packet -> Int
sumVersions (Packet version (Operator _ packets)) = version + (sum . fmap sumVersions $ packets)
sumVersions (Packet version _) = version

runPacket :: Packet -> Int
runPacket (Packet _ (Literal value)) = value
runPacket (Packet _ (Operator t packets))
    | t == 0 = sum $ fmap runPacket packets
    | t == 1 = product subPackets
    | t == 2 = minimum subPackets
    | t == 3 = maximum subPackets
    | t == 5 = case subPackets of
        [a, b] -> if a > b then 1 else 0
        _ -> error "Invalid greater than subpacket"
    | t == 6 = case subPackets of
        [a, b] -> if a < b then 1 else 0
        _ -> error "Invalid lesser than subpacket"
    | t == 7 = case subPackets of
        [a, b] -> if a == b then 1 else 0
        _ -> error "Invalid equals subpacket"
    where
        subPackets = fmap runPacket packets

h2b :: String -> String
h2b = s . r
    where
        r = fst . last . readHex
        s i = showIntAtBase 2 intToDigit (i::Int) ""

padl :: Char -> String -> String
padl c string = if length string < 4 then padl c (c:string) else string

hexToBin :: String -> String
hexToBin "" = ""
hexToBin "\n" = ""
hexToBin string =  (padl '0' . h2b $ take 1 string) ++ hexToBin (tail string)

isBin :: Char -> Bool
isBin c = c == '0' || c == '1'

binToInt :: String -> Int
binToInt = fst . last . readInt 2 isBin digitToInt

bit :: ReadP Char
bit = satisfy isBin

header :: ReadP Int
header = fmap binToInt $ count 3 bit

group :: ReadP Int
group = do
    _ <- char '1'
    fmap binToInt $ count 4 bit

finalGroup :: ReadP Int
finalGroup = do
    _ <- char '0'
    fmap binToInt $ count 4 bit

literal :: ReadP Int
literal = do
    groups <- many group
    final <- finalGroup
    return $ foldl (\acc x -> acc*16 + x) 0 (groups ++ [final])

literalPacket :: ReadP Packet
literalPacket = do
    version <- header
    t <- header
    if t == 4
        then do
            n <- literal
            return $ Packet version (Literal n)
        else
            pfail

bitLengthOperatorPacket :: ReadP Packet
bitLengthOperatorPacket = do
    version <- header
    t <- header
    if t /= 4
        then do
            _ <- char '0'
            n <- fmap binToInt $ count 15 bit
            s <- count n $ get
            packets <- case filter (null . snd) $ readP_to_S (many packet) s of
                ((ps, _):_) -> return ps
                _ -> pfail
            return $ Packet version (Operator t packets)
        else
            pfail

packetNbOperatorPacket :: ReadP Packet
packetNbOperatorPacket = do
    version <- header
    t <- header
    if t /= 4
        then do
            _ <- char '1'
            n <- fmap binToInt $ count 11 bit
            packets <- count n packet
            return $ Packet version (Operator t packets)
        else
            pfail

packet :: ReadP Packet
packet = literalPacket +++ bitLengthOperatorPacket +++ packetNbOperatorPacket

day16 :: String -> String
day16 string = show (sumVersions p, runPacket p)
    where
        p = fst . last . (readP_to_S packet) $ hexToBin string
