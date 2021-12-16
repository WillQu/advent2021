module Main where

import System.Directory

import Day1(day1)
import Day2(day2)
import Day3(day3)
import Day4(day4)
import Day5(day5)
import Day6(day6)
import Day7(day7)
import Day8(day8)
import Day9(day9)
import Day10(day10)
import Day11(day11)
import Day12(day12)
import Day13(day13)
import Day14(day14)
import Day15(day15)
import Day16(day16)

main :: IO ()
main = do
    _ <- dayN 1 day1
    _ <- dayN 2 day2
    _ <- dayN 3 day3
    _ <- dayN 4 day4
    _ <- dayN 5 day5
    _ <- dayN 6 day6
    _ <- dayN 7 day7
    _ <- dayN 8 day8
    _ <- dayN 9 day9
    _ <- dayN 10 day10
    _ <- dayN 11 day11
    _ <- dayN 12 day12
    _ <- dayN 13 day13
    _ <- dayN 14 day14
    _ <- dayN 15 day15
    _ <- dayN 16 day16
    return ()

dayN :: Show a => Int -> (String -> a) -> IO ()
dayN dayNb solution = do
    let file = "day" ++ show dayNb ++ ".input" 
    fileExists <- doesFileExist file
    if not fileExists
        then putStrLn $ "No input for day " ++ show dayNb
        else do
            input <- readFile file
            _ <- putStrLn $ "Day " ++ show dayNb
            print $ solution input
