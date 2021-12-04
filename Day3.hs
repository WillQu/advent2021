module Day3 where

import Data.Char (digitToInt)
import Data.List (transpose, foldl')

day3 :: String -> (String, String)
day3 input = (show $ power l, show $ oxygenGeneratorRating l)
    where l = lines input

mostCommonZero :: String -> Bool
mostCommonZero string =
    let
        num0 = length $ filter (== '0') string
        num1 = length $ filter (== '1') string
    in
        num0 > num1

mostCommon :: String -> Char
mostCommon string = if mostCommonZero string then '0' else '1'

leastCommon :: String -> Char
leastCommon string = if mostCommonZero string then '1' else '0'

gamma :: [String] -> Int
gamma lines = readBin $ map mostCommon $ transpose lines

epsilon :: [String] -> Int
epsilon lines = readBin $ map leastCommon $ transpose lines

power :: [String] -> Int
power lines = gamma lines * epsilon lines

oxygen :: [String] -> Int
oxygen = gas mostCommon 0

co2 :: [String] -> Int
co2 = gas leastCommon 0

gas :: (String -> Char) -> Int -> [String] -> Int
gas criteriaFunction i lines =
    let 
        criteria = criteriaFunction $ map (!! i) lines
        candidates = filter ((== criteria) . (!! i)) lines
    in
        case candidates of
            [x] -> readBin x
            xs -> gas criteriaFunction (i + 1) xs

oxygenGeneratorRating :: [String] -> Int
oxygenGeneratorRating lines = oxygen lines * co2 lines

readBin :: String -> Int
readBin = foldl' (\acc x -> acc * 2 + digitToInt x) 0

