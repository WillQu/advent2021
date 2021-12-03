module Main where

import Day1(runDay1)
import Day2(day2)

main :: IO ()
main = do
    _ <- putStrLn "Day 1"
    _ <- runDay1
    _ <- putStrLn "Day 2"
    input2 <- readFile "day2.input"
    _ <- print $ day2 input2
    return ()
