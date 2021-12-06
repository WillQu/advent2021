module Day6 (day6) where

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)

import Optics

turn :: [Int] -> [Int]
turn fishes =
    let
        born = head fishes
        newList = tail fishes
    in
        (over (ix 6) (+born)) . (over (ix 8) (+born)) $ newList

-- Parsing
readFishes :: ReadP [Int]
readFishes = fmap (fmap read) $ sepBy1 (munch1 isDigit) $ char ','

parseInput :: String -> [Int]
parseInput = fst . last . (readP_to_S readFishes)

-- Solution
compress :: [Int] -> [Int]
compress = foldr put (repeat 0)

put :: Int -> [Int] -> [Int]
put x = over (ix x) (+1)

day6 :: String -> (Int, Int)
day6 input = (simulate 80, simulate 256)
    where simulate n = sum . (take 9) . last . (take $ n + 1) . (iterate turn) . compress $ parseInput input
