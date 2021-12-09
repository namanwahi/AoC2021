module Main where

import Data.Either
import Data.List (sort, union, delete, (\\), intersect)
import Data.Maybe

import Text.Parsec (parse, ParseError, many1, manyTill, count)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (letter, char, string, oneOf)
import Text.Parsec.Combinator (sepBy, sepEndBy)

-- parsing code
regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

parseDisplay :: Parser ([String], [String])
parseDisplay = do
    tokens <- sepBy (many1 (oneOf "abcdefg|")) (char ' ')
    return ((take 10 tokens),  (reverse (take 4 (reverse tokens))))

parseDisplays :: Parser [([String], [String])]
parseDisplays = sepBy parseDisplay (char '\n')
----------------------------------------------------------------------------------------
-- Given a correct list of correct segments, get the corresponding digit. e.g. segmenstToDigits "cf" = 1
segmentsToDigit :: String -> Int
segmentsToDigit segments = fromJust $ lookup (sort segments) [("abcefg", 0), ("cf", 1), ("acdeg", 2), ("acdfg", 3), ("bcdf", 4), ("abdfg", 5), ("abdefg", 6), ("acf", 7), ("abcdefg", 8), ("abcdfg", 9)]

-- get map from wrong segment to correct segment
getSegmentMap :: [String] -> [(Char, Char)]
getSegmentMap uniquePatterns = zip [toA, toB, toC, toD, toE, toF, toG] ['a'..'g']
    where
        isSubset a b = (intersect a b  == a)
        [segments1] = filter (\p -> length p == 2) uniquePatterns
        [segments7] = filter (\p -> length p == 3) uniquePatterns
        [toA] = segments7 \\ segments1
        [segments4] = filter (\p -> length p == 4) uniquePatterns
        [segments9] = filter (\p -> isSubset segments4 (delete toA p)) $ filter (\p -> length p == 6) uniquePatterns
        [toG] = segments9 \\ (toA : segments4)
        segments0or6 = filter (\p -> (length p == 6) && (p /= segments9)) uniquePatterns
        [segments0] = filter (\p -> isSubset segments1 p) segments0or6
        [segments6] = segments0or6 \\ [segments0]
        [toC] = filter (\c -> not (elem c segments6)) segments1
        [toF] = segments1 \\ [toC]
        [segments5] = filter (\p -> (length p == 5) && isSubset p segments6) uniquePatterns
        [toE] = segments6 \\ segments5
        [toD] = filter (\c -> (elem c segments6) && (elem c segments9) && (not (elem c segments0))) ['a'..'g']
        [toB] = segments4 \\ [toC, toD, toF]

-- decode output digits
decodeOutput :: [(Char, Char)] -> [String] -> Int
decodeOutput segmentMap = read . concat . (map show) . (map (decodeDigit segmentMap))
    where
        decodeDigit segmentMap = segmentsToDigit . (map (\c -> (fromJust $ lookup c segmentMap)))

main :: IO ()
main = do
    input <- readFile "app/input.txt"
    let displays = fromRight [] (regularParse parseDisplays input) :: [([String], [String])]
    -- part 1
    print $ sum $ map length $ map (filter (\l -> elem l [2,3,4,7])) $ map (\(_, o) -> map length o) displays
    -- part 2
    print $ sum . map (\(uniquePatterns, output) -> decodeOutput (getSegmentMap uniquePatterns) output) $ displays