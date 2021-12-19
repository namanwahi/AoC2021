module Main where

import Data.List
import Data.Maybe
import Data.Char (digitToInt)

-- Binary Tree recursive data structure
data Tree = Leaf Int | Node Tree Tree
-- Node position ['l'|'r']
type NodePosition = [Char]

instance Show Tree where
    show (Leaf i) = show i
    show (Node t1 t2) = "[" ++ show t1 ++ "," ++ show t2 ++ "]"

parseTree :: String -> Tree
parseTree tokens = parseTree' tokens []
    where
        parseTree' [] [tree] = tree
        parseTree' ('[': tokens) stack = parseTree' tokens stack
        parseTree' (',': tokens) stack = parseTree' tokens stack
        parseTree' (']': tokens) (treeA : treeB : stack) = parseTree' tokens (Node treeB treeA : stack)
        parseTree' (dig: tokens) stack = parseTree' tokens ((Leaf (digitToInt dig)) : stack)

leafVal :: Tree -> Int
leafVal (Leaf i) = i

-- left to right traversal of node positions
getNodePositions :: Tree -> [(NodePosition, Int)]
getNodePositions = getNodePositions' ""
    where
        getNodePositions' pos (Leaf i) = [(pos, i)]
        getNodePositions' pos (Node t1 t2) = getNodePositions' (pos ++ "l") t1 ++ getNodePositions' (pos ++ "r") t2

getNodeAtPosition :: Tree -> NodePosition -> Tree
getNodeAtPosition t [] = t
getNodeAtPosition (Node t1 t2) ('l' : rest) = getNodeAtPosition t1 rest
getNodeAtPosition (Node t1 t2) ('r' : rest) = getNodeAtPosition t2 rest

replaceNodeAtPosition :: Tree -> NodePosition -> Tree -> Tree
replaceNodeAtPosition _ [] newNode = newNode
replaceNodeAtPosition (Node t1 t2) ('l': nodePosition) newNode = Node (replaceNodeAtPosition t1 nodePosition newNode) t2
replaceNodeAtPosition (Node t1 t2) ('r': nodePosition) newNode = Node t1 (replaceNodeAtPosition t2 nodePosition newNode)

findToExplode :: Tree -> Maybe NodePosition
findToExplode t = case explodeLeftPost of
                                Just (leftPos, rPos) -> Just (removeLast leftPos)
                                Nothing -> Nothing
    where
        positions = map fst $ getNodePositions t
        explodeLeftPost = find (\(lPos, rPos) -> (length lPos) > 4 && samePair lPos rPos) (zip positions (tail positions))
        removeLast = reverse . (tail . reverse)
        samePair lPos rPos = ((tail . reverse) lPos) == ((tail . reverse) rPos)

findNeighbours :: Tree -> NodePosition -> (Maybe NodePosition, Maybe NodePosition)
findNeighbours t nodePosition = (leftPosition, rightPosition)
    where
        positions = map fst $ getNodePositions t :: [NodePosition]
        leftPositionPair = filter (\(left, current) -> current == nodePosition) (zip positions (tail positions)) :: [(NodePosition, NodePosition)]
        leftPosition = if leftPositionPair == [] then Nothing else Just ((fst . head) leftPositionPair) :: Maybe NodePosition

        rightPositionPair = filter (\(current, right) -> current == nodePosition) (zip positions (tail positions)) :: [(NodePosition, NodePosition)]
        rightPosition = if rightPositionPair == [] then Nothing else Just ((snd . head) rightPositionPair) :: Maybe NodePosition

canExplode :: Tree -> Bool
canExplode = (isJust . findToExplode)

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

splitVal :: Int -> (Int, Int)
splitVal n
    | n `mod` 2 == 0 = (n `div` 2, n `div` 2)
    | otherwise = (n `div` 2, (n `div` 2 + 1))

canSplit :: Tree -> Bool
canSplit = isJust . find (>=10) . map snd . getNodePositions

split :: Tree -> Tree
split t = replaceNodeAtPosition t nodePosition (Node (Leaf lVal) (Leaf rVal))
    where
        (nodePosition, val) = (head . filter (\(pos, val) -> val >= 10) . getNodePositions) t
        (lVal, rVal) = splitVal val

reduce :: Tree -> Tree
reduce t
    | canExplode t = explode t
    | canSplit t = split t
    | otherwise = t

main :: IO ()
main = do
    let t = parseTree "[[[[[9,8],1],2],3],4]"
    --let t = parseTree "[7,[6,[5,[4,[3,2]]]]]"
    --let t = parseTree "[[6,[5,[4,[3,2]]]],1]"
    --let t = parseTree "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"
    --let t = parseTree "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
    let t = parseTree "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"
    print $ last $ take 10 $ iterate reduce t