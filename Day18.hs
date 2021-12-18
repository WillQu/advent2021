module Day18 where

import Data.Char (isDigit)
import Control.Monad
import Control.Monad.State.Lazy

import Text.ParserCombinators.ReadP

import Control.Monad.Loops

data Tree = Leaf Int | Branch Tree Tree

instance Show Tree where
    show (Leaf i) = show i
    show (Branch a b) = "["++show a++","++show b++"]"

magnitude :: Tree -> Int
magnitude (Leaf x) = x
magnitude (Branch a b) = 3 * magnitude a + 2 * magnitude b

data Context = L Tree | R Tree

contextTree :: Context -> Tree
contextTree (L t) = t
contextTree (R t) = t

rebuildTree :: Context -> Tree -> Tree
rebuildTree (L rt) lt = Branch lt rt
rebuildTree (R lt) rt = Branch lt rt

data Zipper = Zipper {
    tree :: Tree,
    contexts :: [Context]
}

type ZipState = State Zipper

left :: ZipState ()
left = do
    t <- gets tree
    cs <- gets contexts
    case t of
        Branch l r -> put (Zipper l ((L r):cs))
        _ -> error "No left"

isLeft :: ZipState Bool
isLeft = gets $ \(Zipper _ contexts) -> case contexts of
    ((L _):_) -> True
    _ -> False

isRight :: ZipState Bool
isRight = gets $ \(Zipper _ contexts) -> case contexts of
    ((R _):_) -> True
    _ -> False

isTop :: ZipState Bool
isTop = gets $ null . contexts

bottomLeft :: ZipState ()
bottomLeft = do
    t <- gets tree
    case t of
        Leaf _ -> return ()
        _ -> left >> bottomLeft

right :: ZipState ()
right = do
    t <- gets tree
    cs <- gets contexts
    case t of
        Branch l r -> put (Zipper r ((R l):cs))
        _ -> error "No right"

bottomRight :: ZipState ()
bottomRight = do
    t <- gets tree
    case t of
        Leaf _ -> return ()
        _ -> right >> bottomRight

up :: ZipState ()
up = do
    t <- gets tree
    cs <- gets contexts
    case cs of  
        (c:rest) -> put (Zipper (rebuildTree c t) rest)
        _ -> error "No up"

top :: ZipState ()
top = whileM_ (fmap not isTop) up

isBranch :: ZipState Bool
isBranch = gets $ \zipper -> case tree zipper of
    Branch _ _ -> True
    Leaf _ -> False

leftExplode :: ZipState Bool
leftExplode = do
    currentLeaf <- gets tree
    currentLeafIsLeft <- isLeft
    if currentLeafIsLeft
    then do
        _ <- whileM isLeft up
        top <- isTop
        _ <- if top
        then return ()
        else do
            _ <- up
            _ <- left
            _ <- bottomRight
            leftLeaf <- gets tree
            case (currentLeaf, leftLeaf) of
                (Leaf i, Leaf j) -> modify $ \zipper -> zipper { tree = Leaf (i+j) }
                _ -> error $ "Expected leaves but got " ++ show (currentLeaf, leftLeaf)
        return True
    else return False

rightExplode :: ZipState Bool
rightExplode = do
    currentLeaf <- gets tree
    currentLeafIsRight <- isRight
    if currentLeafIsRight
    then do
        _ <- whileM isRight up
        top <- isTop
        _ <- if top
        then return ()
        else do
            _ <- up
            _ <- right
            _ <- bottomLeft
            rightLeaf <- gets tree
            case (currentLeaf, rightLeaf) of
                (Leaf i, Leaf j) -> modify $ \zipper -> zipper { tree = Leaf (i+j) }
                _ -> error $ "Expected leaves but got " ++ show (currentLeaf, rightLeaf)
        return True
    else return False

cleanExplode :: ZipState Bool
cleanExplode = do
    _ <- up
    _ <- modify $ \zipper -> zipper { tree = Leaf 0 }
    return True

searchAndExplode :: ZipState Bool -> ZipState Bool
searchAndExplode explosionStep = do
    contexts <- gets contexts
    branch <- isBranch
    if length contexts > 4
    then do 
        explosionStep
    else if branch then do
        _ <- left
        exploded <- searchAndExplode explosionStep
        if exploded then return True else do
            _ <- up
            _ <- right
            result <- searchAndExplode explosionStep
            _ <- when (not result) up
            return result
    else return False

explode :: ZipState Bool
explode = do
    _ <- searchAndExplode leftExplode
    _ <- top
    _ <- searchAndExplode rightExplode
    _ <- top
    searchAndExplode cleanExplode

split :: ZipState Bool
split = do
    tree <- gets tree
    case tree of
        Leaf i
            | i >= 10 -> do
                let a = i `div` 2
                let b = if a*2 == i then a else a + 1
                _ <- modify $ \zipper -> zipper { tree = Branch (Leaf a) (Leaf b) }
                return True
            | otherwise -> return False
        _ -> do
            _ <- left
            lsplit <- split
            _ <- up
            if lsplit then return True else do
                _ <- right
                rsplit <- split
                _ <- up
                return $ rsplit

reduce :: ZipState ()
reduce = do
    _ <- top
    exploded <- explode
    if exploded then reduce else do
        _ <- top
        splitted <- split
        if splitted then reduce else return ()

add :: Tree -> Tree -> Tree
add a b =
    let 
        nonReduced = Branch a b
        result = runState (reduce >> top) $ Zipper nonReduced []
    in tree $ snd result

addAll :: Tree -> [Tree] -> [Int]
addAll tree trees = fmap (magnitude . add tree) trees ++ fmap (magnitude . (flip add) tree) trees

maxAdd :: [Tree] -> Int
maxAdd [] = 0
maxAdd [_] = 0
maxAdd (tree:trees) = max (maximum (addAll tree trees)) (maxAdd trees)

leafReadP :: ReadP Tree
leafReadP = fmap (Leaf . read) $ munch1 isDigit

branchReadP :: ReadP Tree
branchReadP = between (char '[') (char ']') $ do
    left <- branchReadP +++ leafReadP
    _ <- char ','
    right <- branchReadP +++ leafReadP
    return $ Branch left right

inputReadP :: ReadP [Tree]
inputReadP = sepBy1 branchReadP $ char '\n'

parse :: String -> [Tree]
parse = fst . last . readP_to_S inputReadP

day18 :: String -> String
day18 string = show (magnitude $ foldl1 add input, maxAdd input)
    where
        input = parse string
