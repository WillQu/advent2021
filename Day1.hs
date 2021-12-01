module Day1(runDay1) where

runDay1 :: IO ()
runDay1 = do
        input <- readFile "day1.input"
        putStrLn . solve $ input
        putStrLn . solve2 $ input

solve :: String -> String
solve = show . count . parse

solve2 :: String -> String
solve2 = show . count2 . parse

parse :: String -> [Int]
parse input = map read $ lines input

count :: [Int] -> Int
count (x:y:xs) = (if y > x then 1 else 0) + count (y:xs)
count _ = 0

count2 :: [Int] -> Int
count2 list
        | length list >= 4 = (if y > x then 1 else 0) + (count2 $ tail list)
        | otherwise = 0
                where
                        x = sum $ take 3 list
                        y = sum $ take 3 $ tail list
