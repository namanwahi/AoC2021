module Main where

import Data.Either

import Debug.Trace
import Data.Maybe
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

import Text.Parsec (parse, ParseError, many1, try)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit, char, string)
import Text.Parsec.Combinator (sepBy, choice)

type Point = (Int, Int, Int)

data Instruction = Instruction {
    isOn :: Bool,
    cube :: Cube
} deriving (Show)

data Cube = Cube {
    xBounds :: (Int, Int),
    yBounds :: (Int, Int),
    zBounds :: (Int, Int)
} deriving (Show, Eq, Ord)

allPoints :: Cube -> [Point]
allPoints (Cube{xBounds=(xLow, xHigh), yBounds=(yLow, yHigh), zBounds=(zLow, zHigh)}) = [(x, y, z)|x<-[xLow..xHigh], y<-[yLow..yHigh], z<-[zLow..zHigh]]

volume :: Cube -> Int
volume (Cube{xBounds=(xLow, xHigh), yBounds=(yLow, yHigh), zBounds=(zLow, zHigh)}) = (1 + xHigh - xLow) * (1 + yHigh - yLow) * (1 + zHigh - zLow)

-- parsing code
regularParse :: Parser a -> String -> a
regularParse p s = case parse p "" s of
                        Right parsed -> parsed
                        Left e -> error (show e)

parseInteger :: Parser Int
parseInteger = choice [parsePositive, parseNegative]
    where
        parsePositive = do { n <- many1 digit ; return (read n) }
        parseNegative = do { char '-' ; n <- many1 digit ; return (- (read n)) }

parseRange :: Parser (Int, Int)
parseRange = do
    lower <- parseInteger
    string ".."
    upper <- parseInteger
    return (lower, upper)


parseInstruction :: Parser Instruction
parseInstruction = do
    onOrOff <- choice [try (string "on"), string "off"]
    string " x="
    xRange <- parseRange
    string ",y="
    yRange <- parseRange
    string ",z="
    zRange <- parseRange
    return (Instruction{isOn=(onOrOff == "on"), cube=Cube{xBounds=xRange, yBounds=yRange, zBounds=zRange}})

-- instructions intersect
doIntersect :: Cube -> Cube -> Bool
doIntersect c1 c2 = (xHigh1 >= xLow2) && (xHigh2 >= xLow1) && (yHigh1 >= yLow2) && (yHigh2 >= yLow1) && (zHigh1 >= zLow2) && (zHigh2 >= zLow1)
    where
        Cube{xBounds=(xLow1, xHigh1), yBounds=(yLow1, yHigh1), zBounds=(zLow1, zHigh1)} = c1
        Cube{xBounds=(xLow2, xHigh2), yBounds=(yLow2, yHigh2), zBounds=(zLow2, zHigh2)} = c2


-- does first cube contain second
contains :: Cube -> Cube -> Bool
contains c1 c2 = (xLow1 <= xLow2) && (yLow1 <= yLow2) &&  (zLow1 <= zLow2) && (xHigh1 >= xHigh2) && (yHigh1 >= yHigh2) && (zHigh1 >= zHigh2)
    where
        Cube{xBounds=(xLow1, xHigh1), yBounds=(yLow1, yHigh1), zBounds=(zLow1, zHigh1)} = c1
        Cube{xBounds=(xLow2, xHigh2), yBounds=(yLow2, yHigh2), zBounds=(zLow2, zHigh2)} = c2

-- decompose cubes into non overlapping regions
decomposeCubes :: [Cube] -> [Cube]
decomposeCubes cubes
    | (length cubes) /= (length (nub cubes)) = error "duplicate cubes"
decomposeCubes cubes
    | null intersectPairs = cubes
    | otherwise = []
    where
        intersectPairs = [ (c1, c2) | c1<-cubes, c2<-cubes, c1 /= c2, doIntersect c1 c2]
        (c1, c2) = head intersectPairs

decompose:: Cube -> Cube -> [Cube]
decompose c1 c2
    | not (doIntersect c1 c2) = error "dont intersect"
    | otherwise = nub allCubes
    where
        allCubes = [Cube{xBounds=xb, yBounds=yb, zBounds=zb} | xb<-newXRanges, yb<-newYRanges, zb<-newZRanges]
        newXRanges = splitRanges (xBounds c1) (xBounds c2)
        newYRanges = splitRanges (yBounds c1) (yBounds c2)
        newZRanges = splitRanges (zBounds c1) (zBounds c2)


splitRanges :: (Int, Int) -> (Int, Int) -> [(Int,Int)]
splitRanges r1 r2
    | r1 == r2 = [r1, r2]
splitRanges (low1, high1) (low2, high2) = sortedInclusiveBounds
    where
        intersectingPoints = union [low1..high1] [low2..high2]
        minIntersect = minimum intersectingPoints
        maxIntersect = maximum intersectingPoints
        sortedBoundPoints = (sort . nub) [low1, high1, low2, high2, minIntersect, maxIntersect]
        sortedInclusiveBounds = zip sortedBoundPoints (tail sortedBoundPoints)

findCompositeCubes :: Cube -> [Cube] -> [Cube]
findCompositeCubes cube allDecomposedCubes
    | vol /= decompVol = error ("interval composition error" ++ (show vol) ++ " " ++ (show decompVol))
    | otherwise = decomped
    where
        decomped = filter (\c -> contains cube c) allDecomposedCubes
        vol = volume cube
        decompVol = sum $ map volume decomped

main :: IO ()
main = do
    input <- readFile "app/test_input.txt"
    let instructions = (map (regularParse parseInstruction) . lines) input
    let cubes = map cube instructions
    --mapM_ (putStrLn . show) cubes
    let c1 = head cubes
    let c2 = head (tail cubes)
    let decomped = decompose c1 c2
    mapM_ print decomped
    print $ length decomped
    print c1
    print c2
    print "------------"
    print $ splitRanges (-5, 47) (-44, 5)
    mapM_ print (findCompositeCubes c1 decomped)
    {--
    print $ splitRanges (10, 15) (12, 100)
    print $ splitRanges (4, 10) (3, 6)
    print $ splitRanges (10, 15) (12, 100)
    print $ splitRanges (0, 10) (2, 5)
    print $ splitRanges (2, 5) (0, 10)
    print $ splitRanges (2, 5) (5, 10)
    print $ splitRanges (2, 5) (5, 6)
--}