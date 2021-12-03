module Day2(day2) where

day2 :: String -> (String, String)
day2 input = 
    let
        parsedInput = parse input
    in (show $ solve parsedInput, show $ solve2 parsedInput)

data Direction = Forward | Up | Down
data Command = Command Direction Int

data Status = Status {
    depth :: Int,
    x :: Int,
    aim :: Int
}

parse :: String -> [Command]
parse input = map parseLine $ lines input

parseLine :: String -> Command
parseLine line = case words line of
    ["forward", x] -> Command Forward $ read x
    ["up", x] -> Command Up $ read x
    ["down", x] -> Command Down $ read x
    _ -> error $ "Unknown command " ++ line

solve :: [Command] -> Int
solve commands = x * y
    where
        (x, y) = foldl move (0, 0) commands

solve2 :: [Command] -> Int
solve2 commands = x * y
    where
        Status y x _ = foldl move2 (Status 0 0 0) commands

move :: (Int, Int) -> Command -> (Int, Int)
move (x, y) (Command Forward dx) = (x + dx, y)
move (x, y) (Command Up dy) = (x, y - dy)
move (x, y) (Command Down dy) = (x, y + dy)

move2 :: Status -> Command -> Status
move2 (Status depth x aim) (Command Forward dx) = Status (depth + (dx*aim)) (x + dx) aim
move2 (Status depth x aim) (Command Up dy) = Status depth x (aim - dy)
move2 (Status depth x aim) (Command Down dy) = Status depth x (aim + dy)
