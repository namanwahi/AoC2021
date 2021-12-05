module Main where

import Data.Either

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator

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

-- Part 1

-- Convert non-diagonal vent line from (startPoint, endPoint) format to a list of all points within
pointsWithin :: (Point, Point) -> [Point]
pointsWithin ((x1, y1), (x2, y2))
    | x1 == x2 = map (\y -> (x1, y)) [(min y1 y2)..(max y1 y2)]
    | y1 == y2 = map (\x -> (x, y1)) [(min x1 x2)..(max x1 x2)]
    | otherwise = []

-- Maps and concatenates all the points within non diagonal lines
allPointsWithin :: [(Point, Point)] -> [Point]
allPointsWithin = concat . (map pointsWithin)

-- Finds overlapping points
overlappingPoints :: [Point] -> [Point]
overlappingPoints points = overlappingPoints' points []
    where
        overlappingPoints' [] overlappingAcc = overlappingAcc
        overlappingPoints' (p : ps) overlappingAcc
            | duplicatesInTail = overlappingPoints' (filter (/=p) ps) (p : overlappingAcc)
            | otherwise = overlappingPoints' ps overlappingAcc
            where
                duplicatesInTail = p `elem` ps

main :: IO ()
main = do
    input <- readFile "app/input.txt"
    let parsedInput = fromRight [] (regularParse (ventLines) input)
    print $ length $ overlappingPoints $ allPointsWithin parsedInput
