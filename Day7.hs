module Day7 (day7) where

import Data.Char (isDigit)
import Data.List (sort)
import Text.ParserCombinators.ReadP

-- Parsing
parse :: String -> [Int]
parse = fst . last . (readP_to_S readInput)

readInput :: ReadP [Int]
readInput = sepBy1 (fmap read $ munch1 isDigit) $ char ','

-- Solution
median :: [Int] -> Int
median list = list !! (length list `div` 2)

mean :: [Int] -> Int
mean list = sum list `div` length list

fuel :: Int -> [Int] -> Int
fuel x = sum . (fmap (\n -> abs $ x - n))

fuel2 :: Int -> [Int] -> Int
fuel2 x = sum . (fmap (\n -> oneFuel . abs $ x - n))

oneFuel :: Int -> Int
oneFuel x = sum [1 .. x]

day7 :: String -> (Int, Int)
day7 input = (fuel m list, fuel2 a list)
    where
        list = sort $ parse input
        m = median list
        a = mean list
