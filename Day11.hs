{-# LANGUAGE TemplateHaskell #-}

module Day11 where

import Data.Char (digitToInt, isDigit)
import Control.Monad.State.Lazy

import Data.Array.IArray
import Text.ParserCombinators.ReadP hiding (get)

import Optics

data Cell = Cell {
    _value :: Int,
    _flashed :: Bool
} deriving Show

makeLenses ''Cell

flashCell :: Cell -> Cell
flashCell (Cell value flashed) = if value >= 10 then Cell 0 True else Cell value flashed

cell :: Int -> Cell
cell x = Cell x False

type Grid = Array (Int, Int) Cell

incr :: State Grid ()
incr = modify $ (over (traversed % value) (+1)) . (set (traversed % flashed) False)

incrPoint :: (Int, Int) -> State Grid ()
incrPoint i = modify $ \grid -> if preview (ix i % flashed) grid == Just False then over (ix i % value) (+1) grid else grid

flashes :: State Grid [(Int, Int)]
flashes = do
    result <- gets $ fmap fst . filter (\entry -> view (_2 % value) entry >= 10 && not (view (_2 % flashed) entry)) . assocs
    _ <- modify $ fmap flashCell
    return result

propagate :: (Int, Int) -> State Grid ()
propagate point = do
    grid <- get
    let increments = filter (/= (0,0)) [(x, y) | x <- [-1 .. 1], y <- [-1 .. 1]]
    let points = filter (inGrid grid) $ fmap (add point) increments
    _ <- traverse incrPoint points
    return ()

propagates :: [(Int, Int)] -> State Grid Int
propagates [] = return 0
propagates (p:ps) = do
    _ <- propagate p
    fs <- flashes
    fmap (+ length fs) $ propagates (ps ++ fs)

step :: State Grid Int
step = do
    _ <- incr
    fs <- flashes
    fmap (+ length fs) $ propagates fs

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (x, y) (x', y') = (x + x', y + y')

inGrid :: Grid -> (Int, Int) -> Bool
inGrid grid (x, y) =
    let
        ((xMin, yMin), (xMax, yMax)) = bounds grid
    in
        x >= xMin && x <= xMax && y >= yMin && y <= yMax

readPLine :: ReadP [Int]
readPLine = fmap (fmap digitToInt) $ munch1 isDigit

readPInput :: ReadP Grid
readPInput = do
    lists <- sepBy1 readPLine $ char '\n'
    return . fmap cell . listArray ((0, 0), (9, 9)) $ concat lists

parse :: String -> Grid
parse = fst . last . readP_to_S readPInput

solution1 :: Grid -> Int
solution1 = sum . fst . runState (replicateM 100 step)

stepToFlash :: State Grid Int
stepToFlash = do
    _ <- step
    allFlash <- gets $ andOf (folded % flashed)
    if allFlash then return 1 else fmap (+1) stepToFlash

solution2 :: Grid -> Int
solution2 = fst . runState stepToFlash

day11 :: String -> (Int, Int)
day11 string = (solution1 input, solution2 input)
    where
        input = parse string
