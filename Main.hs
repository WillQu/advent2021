module Main where

import Day1(runDay1)
import Day2(day2)
import Day3(day3)
import Day4(day4)
import Day5(day5)
import Day6(day6)

main :: IO ()
main = do
    _ <- putStrLn "Day 1"
    _ <- runDay1
    _ <- putStrLn "Day 2"
    input2 <- readFile "day2.input"
    _ <- print $ day2 input2
    _ <- putStrLn "Day 3"
    input3 <- readFile "day3.input"
    _ <- print $ day3 input3
    _ <- putStrLn "Day 4"
    input4 <- readFile "day4.input"
    _ <- print $ day4 input4
    _ <- putStrLn "Day 5"
    input5 <- readFile "day5.input"
    _ <- print $ day5 input5
    _ <- putStrLn "Day 6"
    input6 <- readFile "day6.input"
    _ <- print $ day6 input6
    return ()
