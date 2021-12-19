module Main where

import Data.List
import Data.Maybe
import Data.Char (digitToInt)

-- Binary Tree recursive data structure
data Tree = Leaf Int | Node Tree Tree
-- Node position ['l'|'r']
type NodePosition = [Char]

-- Convert Tree back to original string - debugging
instance Show Tree where
    show (Leaf i) = show i
    show (Node t1 t2) = "[" ++ show t1 ++ "," ++ show t2 ++ "]"

-- simplified shift-reduce parser to make trees
parseTree :: String -> Tree
parseTree tokens = parseTree' tokens []
    where
        parseTree' [] [tree] = tree
        parseTree' ('[': tokens) stack = parseTree' tokens stack
        parseTree' (',': tokens) stack = parseTree' tokens stack
        parseTree' (']': tokens) (treeA : treeB : stack) = parseTree' tokens (Node treeB treeA : stack)
        parseTree' (dig: tokens) stack = parseTree' tokens ((Leaf (digitToInt dig)) : stack)

-- extract value from leaf node
leafVal :: Tree -> Int
leafVal (Leaf i) = i

-- left to right traversal of node positions
getNodePositions :: Tree -> [(NodePosition, Int)]
getNodePositions = getNodePositions' ""
    where
        getNodePositions' pos (Leaf i) = [(pos, i)]
        getNodePositions' pos (Node t1 t2) = getNodePositions' (pos ++ "l") t1 ++ getNodePositions' (pos ++ "r") t2

-- Find the node of the tree at a given nodeposition
getNodeAtPosition :: Tree -> NodePosition -> Tree
getNodeAtPosition t [] = t
getNodeAtPosition (Node t1 t2) ('l' : rest) = getNodeAtPosition t1 rest
getNodeAtPosition (Node t1 t2) ('r' : rest) = getNodeAtPosition t2 rest

-- Replace a node of a tree at a given node position with a new node
replaceNodeAtPosition :: Tree -> NodePosition -> Tree -> Tree
replaceNodeAtPosition _ [] newNode = newNode
replaceNodeAtPosition (Node t1 t2) ('l': nodePosition) newNode = Node (replaceNodeAtPosition t1 nodePosition newNode) t2
replaceNodeAtPosition (Node t1 t2) ('r': nodePosition) newNode = Node t1 (replaceNodeAtPosition t2 nodePosition newNode)

-- Find node position to explode
findToExplode :: Tree -> Maybe NodePosition
findToExplode t = case explodeLeftPost of
                                Just (leftPos, rPos) -> Just (removeLast leftPos)
                                Nothing -> Nothing
    where
        positions = map fst $ getNodePositions t
        explodeLeftPost = find (\(lPos, rPos) -> (length lPos) > 4 && samePair lPos rPos) (zip positions (tail positions))
        removeLast = reverse . (tail . reverse)
        samePair lPos rPos = ((tail . reverse) lPos) == ((tail . reverse) rPos)

-- find points to the left and right of the node position (Note: not children of the node, left and right of the order they appear in the expression)
findNeighbours :: Tree -> NodePosition -> (Maybe NodePosition, Maybe NodePosition)
findNeighbours t nodePosition = (leftPosition, rightPosition)
    where
        positions = map fst $ getNodePositions t :: [NodePosition]
        leftPositionPair = filter (\(left, current) -> current == nodePosition) (zip positions (tail positions)) :: [(NodePosition, NodePosition)]
        leftPosition = if leftPositionPair == [] then Nothing else Just ((fst . head) leftPositionPair) :: Maybe NodePosition

        rightPositionPair = filter (\(current, right) -> current == nodePosition) (zip positions (tail positions)) :: [(NodePosition, NodePosition)]
        rightPosition = if rightPositionPair == [] then Nothing else Just ((snd . head) rightPositionPair) :: Maybe NodePosition

-- Can a tree explode
canExplode :: Tree -> Bool
canExplode = (isJust . findToExplode)

-- Explode a tree at its leftmost explode point
explode :: Tree -> Tree
explode t = t'''
    where
        explodePos = fromJust $ findToExplode t
        toExplode@(Node (Leaf leftVal) (Leaf rightVal)) = getNodeAtPosition t explodePos
        t' = replaceNodeAtPosition t explodePos (Leaf 0)
        explodeNeighbours@(leftOfExplode, rightOfExplode) = findNeighbours t' explodePos
        t'' = case leftOfExplode of
                Just lPos -> replaceNodeAtPosition t' lPos (Leaf (leftVal + (leafVal (getNodeAtPosition t' lPos))))
                Nothing -> t'
        t''' = case rightOfExplode of
                Just rPos -> replaceNodeAtPosition t'' rPos (Leaf (rightVal + (leafVal (getNodeAtPosition t'' rPos))))
                Nothing -> t''

-- split an integer
splitVal :: Int -> (Int, Int)
splitVal n
    | n `mod` 2 == 0 = (n `div` 2, n `div` 2)
    | otherwise = (n `div` 2, (n `div` 2 + 1))

-- Can a tree be split at a node
canSplit :: Tree -> Bool
canSplit = isJust . find (>=10) . map snd . getNodePositions

-- Split a tree at its leftmost possible split point
split :: Tree -> Tree
split t = replaceNodeAtPosition t nodePosition (Node (Leaf lVal) (Leaf rVal))
    where
        (nodePosition, val) = (head . filter (\(pos, val) -> val >= 10) . getNodePositions) t
        (lVal, rVal) = splitVal val

-- Reduce a tree until it can't be reduced further
reduce :: Tree -> Tree
reduce t
    | canExplode t = reduce (explode t)
    | canSplit t = reduce (split t)
    | otherwise = t

-- Add two snailnumber trees
add :: Tree -> Tree -> Tree
add t1 t2 = reduce $ Node (reduce t1) (reduce t2)

-- Magnitude of a snailnumber tree
magnitude :: Tree -> Int
magnitude (Leaf i) = i
magnitude (Node t1 t2) = (3 * magnitude t1) + (2 * magnitude t2)

main :: IO ()
main = do
    input <- readFile "app/input.txt"
    let ts = (map parseTree . lines) input
    print $ magnitude $ foldl1 add ts
    print $ maximum $ map magnitude [add t1 t2 | t1 <- ts, t2 <- ts]