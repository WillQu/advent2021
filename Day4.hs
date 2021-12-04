module Day4(day4) where

import Data.List (transpose)
import Data.Char (isDigit)
import Data.Maybe (fromJust)
import Text.ParserCombinators.ReadP

import Optics

day4 :: String -> (Int, Int)
day4 input = (scoreFirst game, scoreLast game)
    where
        game = parseBoard input

data Status = Marked | Unmarked deriving (Eq, Show)
data Cell = Cell {
    nb :: Int,
    status :: Status
} deriving Show

cellNb = lens nb $ \s x -> s { nb = x }

data Board = Board [[Cell]] deriving Show

cells = lens (\(Board x) -> x) $ \s x -> Board x

boardScore :: Board -> Int
boardScore = sumOf (cells % each % each % filtered ((== Unmarked) . status) % cellNb)

data Game = Game {
    _numbers :: [Int],
    _boards :: [Board]
} deriving Show

numbers = lens _numbers $ \s x -> s { _numbers = x }
boards = lens _boards $ \s x -> s { _boards = x }

cell :: Int -> Cell
cell x = Cell x Unmarked

mark :: Int -> Cell -> Cell
mark x (Cell y status)
    | x == y = Cell y Marked
    | otherwise = Cell y status

readCell :: ReadP Cell
readCell = fmap (cell . read) $ count 2 $ satisfy (\c -> isDigit c || c == ' ')

readLine :: ReadP [Cell]
readLine = sepBy1 readCell $ many1 $ char ' '

readBoard :: ReadP Board
readBoard = fmap Board $ sepBy1 readLine $ char '\n'

readGame :: ReadP Game
readGame = do
    numbers <- sepBy1 (fmap read $ many1 $ satisfy isDigit) $ char ','
    _ <- string "\n\n"
    boards <- sepBy1 readBoard $ string "\n\n"
    _ <- skipSpaces
    _ <- eof
    return $ Game numbers boards

parseBoard :: String -> Game
parseBoard input = case readP_to_S readGame input of
    [(game, _)] -> game
    x -> error $ "Input parsing error " ++ show x

win :: Board -> Bool
win (Board cells) = condition cells || condition (transpose cells)
    where
        condition = any (all ((== Marked) . status))

gameEnd :: Game -> Bool
gameEnd = anyOf (boards % each) win

gameEndLast :: Game -> Bool
gameEndLast = allOf (boards % each) win

gameTurn :: Game -> Game
gameTurn game =
    let
        nb = head $ view numbers game
    in
        if null $ view numbers game
        then game
        else (over numbers tail) $ (over (boards % each % cells % each % each) (mark nb)) game

scoreFirst :: Game -> Int
scoreFirst game =
    let
        states = takeWhile (not . gameEnd) $ iterate gameTurn game
        lastNb = head $ view numbers $ last states
        finalState = gameTurn $ last states
    in
        lastNb * (boardScore . fromJust $ findOf (boards % each) win finalState)

scoreLast :: Game -> Int
scoreLast game =
    let
        states = takeWhile (not . gameEndLast) $ iterate gameTurn game
        lastNb = head $ view numbers $ last states
        finalState = last states
        losingBoard = fromJust $ findOf (boards % each) (not . win) finalState
    in
        lastNb * (boardScore $ over (cells % each % each) (mark lastNb) losingBoard)
