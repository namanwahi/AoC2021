module Main where

import Data.Either

import Text.Parsec (parse, ParseError, many)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit, char, string)
import Text.Parsec.Combinator (sepBy)

type Point = (Int, Int)

-- parsing code
regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

integer :: Parser Int
integer = do { n <- many digit ; return (read n) }

ventLine :: Parser (Point, Point)
ventLine = do { x1 <- integer; char ','; y1 <- integer; string " -> "; x2 <- integer; char ','; y2 <- integer; return ((x1, y1), (x2, y2)) }

ventLines :: Parser [(Point, Point)]
ventLines = sepBy ventLine (char '\n')

-------------------------------------------------------------------------------------------
-- get integers in a range maintaining the order. (numsBetween 0 5 = [0,1,2,3,4,5], numsBetween 6 3 = [6,5,4,3])
numsBetween :: Int -> Int -> [Int]
numsBetween x y = if x < y then [x..y] else reverse [y..x]

-- Convert non-diagonal vent line from (startPoint, endPoint) format to a list of all points within
pointsWithin :: (Point, Point) -> [Point]
pointsWithin ((x1, y1), (x2, y2))
    | x1 == x2 = map (\y -> (x1, y)) (numsBetween y1 y2)
    | y1 == y2 = map (\x -> (x, y1)) (numsBetween x1 x2)
    -- added for part 2 - diagonal points
    | otherwise = zip (numsBetween x1 x2) (numsBetween y1 y2)

-- Maps and concatenates all the points within non diagonal lines
allPointsWithin :: [(Point, Point)] -> [Point]
allPointsWithin = concat . (map pointsWithin)

-- Finds overlapping points. Slow O(n^2) -- could optimize with Data.Map
overlappingPoints :: [Point] -> [Point]
overlappingPoints points = overlappingPoints' points []
    where
        overlappingPoints' [] overlappingAcc = overlappingAcc
        overlappingPoints' (p : ps) overlappingAcc
            | p `elem` ps  = overlappingPoints' (filter (/=p) ps) (p : overlappingAcc)
            | otherwise = overlappingPoints' ps overlappingAcc

main :: IO ()
main = do
    input <- readFile "app/input.txt"
    let parsedInput = fromRight [] (regularParse (ventLines) input)
    print $ length $ overlappingPoints $ allPointsWithin parsedInput