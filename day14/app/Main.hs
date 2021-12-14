module Main where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

type InsertionRule = ((Char, Char), (Char))
type PairCounts = Map (Char, Char) Int

-- parse insertion rule string
parseInsertionRule :: String -> InsertionRule
parseInsertionRule (left: right: ' ': '-' : '>': ' ': mid: []) = ((left, right), mid)

-- apply a step and update the pair counts based on the insertion rules
step :: [InsertionRule] -> PairCounts -> PairCounts
step insertionRules pairCounts = foldl updatePairCounts Map.empty (Map.keys pairCounts)
    where
        -- adds pair to the newPairCounts based on the "old" pairCounts
        updatePairCounts newPairCounts pair@(left, right)
            | not (pair `elem` (map fst insertionRules)) = Map.insert pair oldPairCount newPairCounts
            | otherwise = (Map.insertWith (+) (mid, right) oldPairCount . Map.insertWith (+) (left, mid) oldPairCount) newPairCounts
            where
                oldPairCount = fromJust $ Map.lookup pair pairCounts
                mid = fromJust $ lookup pair insertionRules

-- Count how many times an element appears
countElemOccurences :: PairCounts -> Char -> Int
countElemOccurences pairCounts elem = max leftCount rightCount
    where
        leftCount  = sum $ map snd $ filter (\(p, _) -> (fst p == elem)) $ Map.toList pairCounts
        rightCount = sum $ map snd $ filter (\(p, _) -> (snd p == elem)) $ Map.toList pairCounts


main :: IO ()
main = do
    input <- readFile "app/input.txt"
    -- parse
    let (initialSequence: "": insertionRuleStrings) = lines input :: [String]
    let insertionRules = map parseInsertionRule insertionRuleStrings:: [InsertionRule]
    let initialPairs = zip initialSequence (tail initialSequence) :: [(Char, Char)]
    let initialPairCounts = Map.fromList (zip initialPairs (repeat 1))

    -- solve
    let lastPairCounts = last $ take (41) $ iterate (step insertionRules) initialPairCounts
    print $ (\counts -> (maximum counts) - (minimum counts)) $ filter (/= 0) $ map (countElemOccurences lastPairCounts) ['A'..'Z']