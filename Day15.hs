module Day15 where

import Data.Char (isDigit, digitToInt)
import Data.Maybe (fromJust, maybe)
import Data.List (find)
import Data.Array.IArray
import Text.ParserCombinators.ReadP

import Data.PriorityQueue.FingerTree
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set

type Pos = (Int, Int)

data Grid = Grid {
    gArray :: Array Pos Int,
    repeat :: Int
}

getPoint :: Grid -> Pos -> Int
getPoint (Grid gArray _) (x, y) =
    let
        (_, (bx, by)) = bounds gArray
        steps = (x `div` (bx + 1)) + (y `div` (by + 1))
    in
        (((gArray ! (x `mod` (bx + 1), y `mod` (by + 1))) + steps - 1) `mod` 9) + 1 

gBounds :: Grid -> (Int, Int)
gBounds (Grid gArray repeat) =
    let
        (_, (bx, by)) = bounds gArray
    in
        ((bx + 1) * repeat - 1, (by + 1) * repeat - 1)

type Queue = PQueue Int [Pos]

lineReadP :: ReadP [Int]
lineReadP = fmap (fmap digitToInt) $ munch1 isDigit

gridReadP :: ReadP (Array Pos Int)
gridReadP = do
    list@(firstLine:restLines) <- sepBy1 lineReadP $ char '\n'
    let bx = length firstLine - 1
    let by = length (firstLine:restLines) - 1
    return . array ((0, 0), (bx, by)) $ [((x, y), list !! y !! x) | x <- [0..bx], y <- [0..by]]

parse :: String -> Array Pos Int
parse = fst . last . readP_to_S gridReadP

neighbors :: Grid -> Pos -> [Pos]
neighbors grid (x, y) =
    let
        (bx, by) = gBounds grid
        isIn (px, py) = px >= 0 && py >= 0 && px <= bx && py <= by
    in
        filter isIn [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]


scorePath :: [Pos] -> Grid -> Int
scorePath path grid = (sum . fmap (getPoint grid) $ init path)

heuristic :: [Pos] -> Grid -> Int
heuristic path@(pos:_) grid = scorePath path grid + ((bx - fst pos) + (by - snd pos))
    where
        (bx, by) = gBounds grid

updateHeuristic :: [Pos] -> Grid -> Int -> Int
updateHeuristic ((x,y):(x',y'):_) grid old = old - (x - x' + y - y') + (getPoint grid (x, y))

findPath :: Grid -> HashSet Pos -> Queue -> Queue
findPath grid visited paths = maybe (findPath grid (Set.insert (head p) visited) next) (singleton 0 . (:p)) (find (== maxB) ns)
    where
        maxB = gBounds grid
        Just ((heur, p), queue) = minViewWithKey paths
        ns = neighbors grid (head p)
        next = if Set.member (head p) visited then queue else foldr insertPath queue $ fmap (:p) ns
        insertPath path = insert (updateHeuristic path grid heur) path

solution grid = scorePath (fst . fromJust . minView . findPath grid Set.empty $ singleton (heuristic [(0,0)] grid) [(0, 0)]) grid

day15 :: String -> String
day15 string = show $ (solution (Grid input 1), solution (Grid input 5))
    where
        input = parse string
