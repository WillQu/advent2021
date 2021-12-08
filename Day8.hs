{-# LANGUAGE TemplateHaskell #-}

module Day8 (day8) where

import Data.List (sort, foldl')
import Text.ParserCombinators.ReadP

import Optics

data Digit = Digit {
    _digitWires :: [Char]
} deriving Show

makeLenses ''Digit

fromDigit :: Digit -> Int
fromDigit (Digit x)
    | length x == 2 = 1
    | length x == 4 = 4
    | length x == 3 = 7
    | length x == 7 = 8
    | otherwise = 0

data Input = Input {
    _digitSet :: [Digit],
    _output :: [Digit]
} deriving Show

makeLenses ''Input

appears :: Input -> Char -> Int
appears input c = length . filter (== c) $ toListOf (digitSet % folded % digitWires % folded) input

appearsN :: Input -> Int -> [Char]
appearsN input n = filter (\c -> appears input c == n) wires

appearsIn :: Input -> Int -> Char -> Bool
appearsIn input nb c = elemOf (digitWires % each) c digit
    where
        (digit:_) = filter (\d -> fromDigit d == nb) $ view digitSet input

-- Appears 8 times
-- Does not appear in 1
a :: Input -> Char
a input = case filter (not . (appearsIn input 1)) $ appearsN input 8 of
    [x] -> x
    _ -> error $ "couldn't read a from " ++ (show $ toListOf (digitSet % folded % digitWires % folded) input)

-- Appears 6 times
b :: Input -> Char
b input = case appearsN input 6 of
    [x] -> x
    _ -> error "couldn't read b"

-- Appears 8 times
-- Appears in 1
c :: Input -> Char
c input = case filter (appearsIn input 1) $ appearsN input 8 of
    [x] -> x
    _ -> error "couldn't read c"

-- Appears 7 times
-- Appears in 4 and 8
d :: Input -> Char
d input = case filter (\c -> appearsIn input 4 c && appearsIn input 8 c) $ appearsN input 7 of
    [x] -> x
    _ -> error "couldn't read d"

-- Appears 4 times
e :: Input -> Char
e input = case appearsN input 4 of
    [x] -> x
    _ -> error "couldn't read e"

-- Appears 9 times
f :: Input -> Char
f input = case appearsN input 9 of
    [x] -> x
    _ -> error "couldn't read f"

-- Appears 7 times
-- Appears in 8
g :: Input -> Char
g input = case filter (\c -> not (appearsIn input 4 c) && appearsIn input 8 c) $ appearsN input 7 of
    [x] -> x
    _ -> error "couldn't read g"

-- Parsing
wires :: [Char]
wires = ['a' .. 'g']

readDigit :: ReadP Digit
readDigit = fmap Digit $ munch1 (`elem` wires)

readDigitSet :: ReadP [Digit]
readDigitSet = sepBy1 readDigit $ char ' '

readInput :: ReadP Input
readInput = do
    set <- readDigitSet
    _ <- string " | "
    output <- readDigitSet
    return $ Input set output

readInputs :: ReadP [Input]
readInputs = sepBy1 readInput $ char '\n'

parse :: String -> [Input]
parse = fst . last . (readP_to_S readInputs)

-- Solution
countSimpleDigits :: [Input] -> Int
countSimpleDigits = length . filter (simpleDigit . fromDigit) . toListOf (folded % output % folded)
    where
        simpleDigit x = x == 1 || x == 4 || x == 7 || x == 8

day8 :: String -> (Int, Int)
day8 string = (solution1, solution2)
    where
        input = parse string
        solution1 = countSimpleDigits input
        solution2 = sum $ fmap decodeInput input
    

fromDigit2 :: Input -> Digit -> Int
fromDigit2 input digit =
    case ifindOf ifolded isLetter segmentReaders of
        Just (i, _) -> i
        Nothing -> error "Couldn't find digit"
    where
        isLetter _ fs = (sort $ fmap ($ input) fs) == (sort $ view digitWires digit)

segmentReaders :: [[(Input -> Char)]]
segmentReaders =
    [
        [a, b, c, e, f, g],
        [c, f],
        [a, c, d, e, g],
        [a, c, d, f, g],
        [b, c, d, f],
        [a, b, d, f, g],
        [a, b, d, e, f, g],
        [a, c, f],
        [a, b, c, d, e, f, g],
        [a, b, c, d, f, g]
    ]

decodeInput :: Input -> Int
decodeInput input = foldl' incr 0 $ fmap (fromDigit2 input) $ view output input
    where
        incr a b = a * 10 + b
