module Main where
import Data.Maybe
import Data.List

-- open char to close char
openToClose :: [(Char, Char)]
openToClose = [('(', ')'), ('[', ']'), ('<', '>'), ('{', '}')]

-- close char to error score
closeToErrorScore :: [(Char, Int)]
closeToErrorScore = [(')', 3), (']', 57), ('>', 25137), ('}', 1197)]

-- close char to complete score
closeToCompleteScore :: [(Char, Int)]
closeToCompleteScore = [(')', 1), (']', 2), ('>', 4), ('}', 3)]

-- get error score from close char
getErrorScore :: Char -> Int
getErrorScore c = fromJust $ lookup c closeToErrorScore

-- get complete score from close char
getCompleteScore :: Char -> Int
getCompleteScore c = fromJust $ lookup c closeToCompleteScore

-- get total score from a series of complete characters
totalCompleteScore :: [Char] -> Int
totalCompleteScore = foldl (\totalScore closeChar -> (totalScore * 5) + (getCompleteScore closeChar)) 0

-- get closing char from open char
getClosing :: Char -> Char
getClosing c = fromJust $ lookup c openToClose

-- is the character an open char
isOpen :: Char -> Bool
isOpen c = c `elem` (map fst openToClose)

-- get first incorrect closing char if one exists, Nothing otherwise
firstIncorrectClosing :: [Char] -> Maybe Char
firstIncorrectClosing line = firstIncorrectClosing' line []
    where
        firstIncorrectClosing' [] _ = Nothing
        firstIncorrectClosing' (c : cs) openStack
            | isOpen c                           = firstIncorrectClosing' cs (c: openStack)
            | openStack == []                    = Just c
            | c /= (getClosing (head openStack)) = Just c
            | otherwise                          = firstIncorrectClosing' cs (tail openStack)

-- get the remaining closing characters to complete the sequence
completeSequence :: [Char] -> [Char]
completeSequence line = completeSequence' line []
    where
        completeSequence' [] openStack =  map getClosing openStack
        completeSequence' (c : cs) openStack
            | isOpen c  = completeSequence' cs (c: openStack)
            | otherwise = completeSequence' cs (tail openStack)

main :: IO ()
main = do
    input <- readFile "app/input.txt"
    print $ sum $ map (getErrorScore . fromJust) $ filter isJust $ map firstIncorrectClosing $ lines input
    let scores = sort $ map (totalCompleteScore . completeSequence) $ filter (\line -> isNothing (firstIncorrectClosing line)) $ lines input
    print $ (scores !! ((length scores) `div` 2))