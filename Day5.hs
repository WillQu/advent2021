{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}

module Day5 (day5) where

import Data.Char (isDigit)
import Text.ParserCombinators.ReadP
import Data.Maybe (maybe)

import GHC.Generics (Generic)
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Optics

data Point = Point {
    _x :: Int,
    _y :: Int
} deriving (Eq, Show, Generic)

instance Hashable Point

makeLenses ''Point

data Segment = Segment {
    _src :: Point,
    _dest :: Point
} deriving Show

makeLenses ''Segment

generatePoints :: Segment -> [Point]
generatePoints segment
    | fromX == toX = fmap (Point fromX) [minY .. maxY]
    | fromY == toY = fmap (\x -> Point x fromY) [minX .. maxX]
    | otherwise = []
    where
        fromX = view (src % x) segment
        toX = view (dest % x) segment
        fromY = view (src % y) segment
        toY = view (dest % y) segment
        minX = min fromX toX
        maxX = max fromX toX
        minY = min fromY toY
        maxY = max fromY toY

generatePoints2 :: Segment -> [Point]
generatePoints2 segment
    | fromX == toX = fmap (Point fromX) yGen
    | fromY == toY = fmap (\x -> Point x fromY) xGen
    | otherwise = zipWith Point xGen yGen
    where
        fromX = view (src % x) segment
        toX = view (dest % x) segment
        fromY = view (src % y) segment
        toY = view (dest % y) segment
        xDir = if toX > fromX then 1 else -1
        yDir = if toY > fromY then 1 else -1
        xGen = [fromX, (fromX + xDir) .. toX]
        yGen = [fromY, (fromY + yDir) .. toY]

type PointMap = HashMap Point Int

addPoint :: Point -> PointMap -> PointMap
addPoint = Map.alter $ fmap Just $ maybe 1 (+1)

addSegment :: Segment -> PointMap -> PointMap
addSegment segment pointMap = foldr addPoint pointMap $ generatePoints segment

addSegment2 :: Segment -> PointMap -> PointMap
addSegment2 segment pointMap = foldr addPoint pointMap $ generatePoints2 segment

generateMap :: [Segment] -> PointMap
generateMap = foldr addSegment Map.empty

generateMap2 :: [Segment] -> PointMap
generateMap2 = foldr addSegment2 Map.empty

-- Parsing
readDigit :: ReadP Int
readDigit = fmap read $ munch1 isDigit

readPoint :: ReadP Point
readPoint = do
    x <- readDigit
    _ <- char ','
    y <- readDigit
    return $ Point x y

readSegment :: ReadP Segment
readSegment = do
    source <- readPoint
    _ <- string " -> "
    dest <- readPoint
    return $ Segment source dest

readInput :: ReadP [Segment]
readInput = do
    result <- sepBy1 readSegment $ char '\n'
    _ <- skipSpaces
    _ <- eof
    return result

parse :: String -> [Segment]
parse input = case readP_to_S readInput input of
    [(result, _)] -> result
    x -> error $ "Parsing error " ++ show x

-- Solution
day5 :: String -> (Int, Int)
day5 input = (countPoints segments, countPoints2 segments)
    where segments = parse input

countPoints :: [Segment] -> Int
countPoints = length . (Map.filter (> 1)) . generateMap

countPoints2 :: [Segment] -> Int
countPoints2 = length . (Map.filter (> 1)) . generateMap2
