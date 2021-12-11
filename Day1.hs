module Day1 (day1) where

day1 :: String -> (Int, Int)
day1 string = (count input, count2 input)
    where
        input = parse string

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
