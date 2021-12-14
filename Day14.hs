module Day14 where

import Data.Char (isAlpha)
import Data.Maybe
import Data.Foldable (minimumBy, maximumBy)
import Data.Function (on)
import qualified Data.HashMap.Strict as Map

import Text.ParserCombinators.ReadP

import Data.MemoTrie

type Rules = Map.HashMap String Char

readRule :: ReadP (String, Char)
readRule = do
    s <- munch isAlpha
    _ <- string " -> "
    char <- get
    return (s, char)

inputReadP :: ReadP (String, Rules)
inputReadP = do
    string <- munch isAlpha
    _ <- skipSpaces
    rules <- sepBy readRule $ char '\n'
    return (string, Map.fromList rules)

parse :: String -> (String, Rules)
parse = fst . last . readP_to_S inputReadP

countElements :: Rules -> ((Int, String) -> Map.HashMap Char Int) -> (Int, String) -> Map.HashMap Char Int
countElements _ _ (0, string) =
    foldr (Map.alter incr) Map.empty $ init string
        where
            incr = Just . maybe 1 (+1)
countElements rules f (n, (a:b:rest)) = Map.unionWith (+) (f ((n-1), expand rules a b)) (f (n, (b:rest)))
countElements _ _ _ = Map.empty

memoCount :: Rules -> (Int, String) -> Map.HashMap Char Int
memoCount rules = memoFix (countElements rules)

expand :: Rules -> Char -> Char -> String
expand rules a b = [a, fromMaybe (error "Didn't find rule") $ Map.lookup [a, b] rules, b]

solution :: Int -> (String, Rules) -> Int
solution steps (s, rules) =
    let
        counts = Map.unionWith (+) ((memoCount rules) (steps, s)) (Map.singleton (last s) 1)
        minC = minimum counts
        maxC = maximum counts
    in
        maxC - minC

solution1 :: (String, Rules) -> Int
solution1 = solution 10

solution2 :: (String, Rules) -> Int
solution2 = solution 40

day14 :: String -> String
day14 string = show (solution1 input, solution2 input)
    where input = parse string
