module Day10 where

import Data.List (foldl', sort)

parse :: String -> String -> Int
parse _ [] = 0
parse stack (x:xs)
    | x `elem` "({[<" = parse (x:stack) xs
    | x `elem` ")}]>" && null stack = 0
    | x `elem` ")}]>" = case (head stack, x) of
        ('(', ')') -> parse (tail stack) xs
        ('{', '}') -> parse (tail stack) xs
        ('[', ']') -> parse (tail stack) xs
        ('<', '>') -> parse (tail stack) xs
        (_, ')') -> 3
        (_, ']') -> 57
        (_, '}') -> 1197
        (_, '>') -> 25137

parse2 :: String -> String -> String
parse2 [] [] = ""
parse2 stack [] = stack
parse2 stack (x:xs)
    | x `elem` "({[<" = parse2 (x:stack) xs
    | null stack = "" 
    | x == ')' && head stack == '(' = parse2 (tail stack) xs
    | x == ']' && head stack == '[' = parse2 (tail stack) xs
    | x == '}' && head stack == '{' = parse2 (tail stack) xs
    | x == '>' && head stack == '<' = parse2 (tail stack) xs
parse2 stack list = ""


score :: Char -> Int
score '(' = 1
score '[' = 2
score '{' = 3
score '<' = 4

parseLine :: String -> Int
parseLine = parse []

parseLine2 :: String -> String
parseLine2 = parse2 []

scoreLine :: String -> Int
scoreLine = (foldl' (\acc c -> acc * 5 + (score c)) 0) . parseLine2

totalScore :: [String] -> Int
totalScore input =
    let
        scores = sort $ filter (/=0) $ fmap scoreLine input
    in
        scores !! (length scores `div` 2)

day10 :: String -> (Int, Int)
day10 string = (sum $ fmap parseLine input, totalScore input)
    where
        input = lines string
