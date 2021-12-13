module Day13 where

import qualified Data.HashSet as Set
import Data.Char (isDigit)
import Data.List (foldl')
import Text.ParserCombinators.ReadP

import Optics hiding (Fold)

type Grid = Set.HashSet (Int, Int)

display :: Grid -> [String]
display grid =
    let
        maxX = maximum . fmap fst $ Set.toList grid
        maxY = maximum . fmap snd $ Set.toList grid
        emptyDisplay = take (maxY + 1) . repeat . take (maxX + 1) $ repeat '.'
    in
        foldr (\coor display -> set (ix (snd coor) % ix (fst coor)) '#' display) emptyDisplay grid

newtype DisplayGrid = DisplayGrid Grid

instance Show DisplayGrid where
    show (DisplayGrid grid) = '\n' : (unlines $ display grid)

data FoldDirection = XFold | YFold

data Fold = Fold {
    direction :: FoldDirection,
    coor :: Int
}

foldInt :: Int -> Int -> Int
foldInt line x =
    if x <= line
    then x
    else 2 * line - x

foldX :: Int -> (Int, Int) -> (Int, Int)
foldX line (x, y) = (foldInt line x, y)

foldY :: Int -> (Int, Int) -> (Int, Int)
foldY line (x, y) = (x, foldInt line y)

foldGrid :: Fold -> Grid -> Grid
foldGrid (Fold direction coor) grid =
    case direction of
        XFold -> Set.map (foldX coor) grid
        YFold -> Set.map (foldY coor) grid

pairReadP :: ReadP (Int, Int)
pairReadP = do
    x <- munch1 isDigit
    _ <- char ','
    y <- munch1 isDigit
    return (read x, read y)

gridReadP :: ReadP Grid
gridReadP = fmap Set.fromList $ sepBy1 pairReadP $ char '\n'

foldReadP :: ReadP Fold
foldReadP = do
    _ <- string "fold along "
    dir <- (fmap (const XFold) $ char 'x') +++ (fmap (const YFold) $ char 'y')
    _ <- char '='
    coor <- munch1 isDigit
    return $ Fold dir (read coor) 

inputReadP :: ReadP (Grid, [Fold])
inputReadP = do
    grid <- gridReadP
    _ <- skipSpaces
    fs <- sepBy1 foldReadP $ char '\n'
    return (grid, fs)

parse :: String -> (Grid, [Fold])
parse = fst . last . readP_to_S inputReadP

solution1 :: (Grid, [Fold]) -> Int
solution1 (grid, (x:_)) = Set.size $ foldGrid x grid

solution2 :: (Grid, [Fold]) -> DisplayGrid
solution2 (grid, fs) = DisplayGrid $ foldl' (\grid f -> foldGrid f grid) grid fs

day13 :: String -> (Int, DisplayGrid)
day13 string = (solution1 input, solution2 input)
    where
        input = parse string
