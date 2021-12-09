{-# LANGUAGE TemplateHaskell #-}

module Day9 (day9) where

import Data.Char (isDigit, digitToInt)
import Data.List (sort)

import Text.ParserCombinators.ReadP

import Data.HashSet (HashSet)
import qualified Data.HashSet as Set

import Optics

type Node = ((Int, Int), Int)

data Basin = Basin {
    _inside :: HashSet Node,
    _edge :: HashSet Node 
} deriving Show

makeLenses ''Basin

initBasin :: Node -> Basin
initBasin n = Basin (Set.singleton n) (Set.singleton n)

expandBasin :: [[Int]] -> Basin -> Basin
expandBasin list basin =
    let
        edges = view edge basin
        newNeighbor e = filter (\n -> snd n /= 9 && not (n `elem` view inside basin || n `elem` view edge basin)) $ listNeighbors (fst e) list
        push e = case newNeighbor e of
            [] -> []
            xs -> if any (\x -> snd x < snd e) xs then [] else xs
        isInside e = not (any (\x -> snd x < snd e) $ newNeighbor e)
        candidates = Set.fromList (Set.toList edges >>= push)
    in
        (set edge candidates) . (over inside (Set.union (Set.filter isInside $ view edge basin))) $ basin

basin :: [[Int]] -> Node -> Basin
basin list = head . dropWhile (not . null . view edge) . iterate (expandBasin list) . initBasin

-- Parsing
lineP :: ReadP [Int]
lineP = fmap (fmap digitToInt) $ munch1 isDigit

inputP :: ReadP [[Int]]
inputP = sepBy1 lineP $ char '\n'

parse :: String -> [[Int]]
parse = fst . last . (readP_to_S inputP)

-- Solution
neighbors :: (Int, Int) -> (Int, Int) -> Bool
neighbors (x, y) (x', y') = (x == x' && abs (y - y') == 1) || (y == y' && abs (x - x') == 1)

listNeighbors :: (Int, Int) -> [[Int]] -> [Node]
listNeighbors target = itoListOf (ifolded <%> ifolded %& ifiltered isNeighbor)
    where
        isNeighbor i _ = neighbors target i

listLowPoints :: [[Int]] -> [Node]
listLowPoints list = itoListOf (ifolded <%> ifolded %& ifiltered isLowPoint) list
    where
        isLowPoint i p = all (> p) . fmap snd $ listNeighbors i list

basins :: [[Int]] -> [Basin]
basins list = fmap (basin list) $ listLowPoints list

solution1 :: [[Int]] -> Int
solution1 = sum . fmap (+1) . fmap snd . listLowPoints

solution2 :: [[Int]] -> Int
solution2 = product . take 3 . reverse . sort . fmap (length . view inside) . basins

day9 :: String -> (Int, Int)
day9 string = (solution1 input, solution2 input)
    where
        input = parse string
