module Day17 where

import Debug.Trace

import Numeric
import Text.ParserCombinators.ReadP

type Vector = (Int, Int)

dspeedx :: Int -> Int
dspeedx x =
    case compare x 0 of
        LT -> x + 1
        EQ -> 0
        GT -> x - 1

dspeedy y = y - 1

dspeed :: Vector -> Vector
dspeed (x, y) = (dspeedx x, dspeedy y)

dpos :: Int -> Int -> Int
dpos dx x = x + dx

dposv :: Vector -> Vector -> Vector
dposv (dx, dy) (x, y) = (x + dx, y + dy)

stepx :: Int -> Int -> (Int, Int)
stepx dx x = (dspeedx dx, dpos dx x)

stepy :: Int -> Int -> (Int, Int)
stepy dy y = (dspeedy dy, dpos dy y)

step :: Vector -> Vector -> (Vector, Vector)
step v p = (dspeed v, dposv v p)

launchx :: (Vector, Vector) -> Int -> (Int, Int)
launchx target speed = result
    where result = until (\x -> inTargetx target x || offLimitx target x || fst x == 0) (\(x, y) -> stepx x y) (speed, 0)

launchy :: (Vector, Vector) -> Int -> (Int, Int)
launchy target speed = result
    where result = until (\x -> inTargety target x || offLimity target x) (\(x, y) -> stepy x y) (speed, 0)

launch :: (Vector, Vector) -> Vector -> (Vector, Vector)
launch target speed = until (\x -> inTarget target x || offLimit target x) (\(x, y) -> step x y) (speed, (0, 0))

offLimitx :: (Vector, Vector) -> (Int, Int) -> Bool
offLimitx ((_, _), (lx, _)) (_, x) = x > lx

offLimity :: (Vector, Vector) -> (Int, Int) -> Bool
offLimity ((_, ly), (_, _)) (_, y) = y < ly

offLimit :: (Vector, Vector) -> (Vector, Vector) -> Bool
offLimit ((_, ly), (lx, _)) (_, (x, y)) = x > lx || y < ly

inTargetx :: (Vector, Vector) -> (Int, Int) -> Bool
inTargetx ((lx, ly), (lx', ly')) (_, x) = x >= lx && x <= lx'

inTargety :: (Vector, Vector) -> (Int, Int) -> Bool
inTargety ((lx, ly), (lx', ly')) (_, y) = y <= ly' && y >= ly

inTarget :: (Vector, Vector) -> (Vector, Vector) -> Bool
inTarget ((lx, ly), (lx', ly')) (_, (x, y)) = x >= lx && x <= lx' && y <= ly' && y >= ly

minX :: (Vector, Vector) -> Int
minX ((x, _), _) = (-1 + (floor . sqrt $ fromIntegral (1 + 8*x))) `div` 2

maxX :: (Vector, Vector) -> Int
maxX (_, (x, _)) = x

minY :: (Vector, Vector) -> Int
minY ((_, y), _) = y

maxY :: (Vector, Vector) -> Int
maxY (_, (_, y)) = y*(y+1) `div` 2

shotsx :: (Vector, Vector) -> [Int]
shotsx input = [minX input .. maxX input]

shotsy :: (Vector, Vector) -> [Int]
shotsy input = [minY input .. maxY input]

shotsInTargetx :: (Vector, Vector) -> [Int]
shotsInTargetx target = filter (inTargetx target . launchx target) $ shotsx target

shotsInTargety :: (Vector, Vector) -> [Int]
shotsInTargety target = filter (inTargety target . launchy target) $ shotsy target

shotsInTarget :: (Vector, Vector) -> [Vector]
shotsInTarget target = traceShow result result
    where result = filter (inTarget target . launch target) [(x, y) | x <- shotsInTargetx target, y <- shotsInTargety target]

day17 = length . shotsInTarget . fst . last . readP_to_S inputReadP

intReadP :: ReadP Int
intReadP = readS_to_P $ readSigned readDec

inputReadP :: ReadP (Vector, Vector)
inputReadP = do
    _ <- string "target area: x="
    x <- intReadP
    _ <- string ".."
    x' <- intReadP
    _ <- string ", y="
    y <- intReadP
    _ <- string ".."
    y' <- intReadP
    _ <- char '\n'
    return ((x, y), (x', y'))
