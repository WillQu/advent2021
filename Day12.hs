{-# LANGUAGE TemplateHaskell #-}

module Day12 where

import Data.Char (isUpper, isLower)
import Data.List (group, sort)
import Text.ParserCombinators.ReadP

import Optics

-- Data
data EdgeType = Start | End | Big | Small deriving (Eq, Show, Ord)

data Edge = Edge {
    _edgeType :: EdgeType,
    _key :: String
} deriving (Eq, Show, Ord)

makeLenses ''Edge

start :: Edge
start = Edge Start "start"

end :: Edge
end = Edge End "end"

type Vertex = (Edge, Edge)

-- Parsing
readPStart :: ReadP Edge
readPStart = fmap (const start) $ string "start"

readPEnd :: ReadP Edge
readPEnd = fmap (const end) $ string "end"

readPBig :: ReadP Edge
readPBig = do
    key <- munch1 isUpper
    return $ Edge Big key

readPSmall :: ReadP Edge
readPSmall = do
    key <- munch1 isLower
    if key == "start" || key == "end"
        then pfail
        else return $ Edge Small key

readPEdge :: ReadP Edge
readPEdge = choice [readPStart, readPEnd, readPBig, readPSmall]

readPVertex :: ReadP Vertex
readPVertex = do
    nodes <- sepBy1 readPEdge $ char '-'
    case nodes of
        [a, b] -> return (a, b)
        _ -> pfail

readPInput :: ReadP [Vertex]
readPInput = sepBy1 readPVertex $ char '\n'

parse :: String -> [Vertex]
parse = fst . last . readP_to_S readPInput

-- Solution
findPaths :: Int -> [Vertex] -> [Edge] -> [[Edge]]
findPaths visits vertices path@(currentEdge:past)
    | currentEdge == end = [path]
    | view edgeType currentEdge == Start && (not . null) past = []
    | view edgeType currentEdge == Small && (not . allowedVisits visits) path = []
    | otherwise = concat . fmap (\edge -> findPaths visits vertices (edge:path)) $ findNextEdges currentEdge vertices 

allowedVisits :: Int -> [Edge] -> Bool
allowedVisits visits path =
    let
        grouped = group . sort $ filter (\edge -> view edgeType edge == Small) path
        duplicates = filter ((> 1) . length) grouped
    in
        length duplicates == 0 || (length duplicates == 1 && (length . head) duplicates <= visits)

findNextEdges :: Edge -> [Vertex] -> [Edge]
findNextEdges edge = concat . fmap (getEdge edge)

getEdge :: Edge -> Vertex -> [Edge]
getEdge edge (x, y)
    | edge == x = [y]
    | edge == y = [x]
    | otherwise = []

countPaths :: Int -> [Vertex] -> Int
countPaths visits vertices = length $ findPaths visits vertices [start]

day12 :: String -> (Int, Int)
day12 string = (countPaths 1 input, countPaths 2 input)
    where
        input = parse string
