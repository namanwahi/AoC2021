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

-- do cubes intersect (not excluding edge piece)
doIntersect :: Cube -> Cube -> Bool
doIntersect c1 c2 = (xHigh1 > xLow2) && (xHigh2 > xLow1) && (yHigh1 > yLow2) && (yHigh2 > yLow1) && (zHigh1 > zLow2) && (zHigh2 > zLow1)
    where
        Cube{xBounds=(xLow1, xHigh1), yBounds=(yLow1, yHigh1), zBounds=(zLow1, zHigh1)} = c1
        Cube{xBounds=(xLow2, xHigh2), yBounds=(yLow2, yHigh2), zBounds=(zLow2, zHigh2)} = c2

-- does first cube contain second
contains :: Cube -> Cube -> Bool
contains c1 c2 = (xLow1 <= xLow2) && (yLow1 <= yLow2) &&  (zLow1 <= zLow2) && (xHigh1 >= xHigh2) && (yHigh1 >= yHigh2) && (zHigh1 >= zHigh2)
    where
        Cube{xBounds=(xLow1, xHigh1), yBounds=(yLow1, yHigh1), zBounds=(zLow1, zHigh1)} = c1
        Cube{xBounds=(xLow2, xHigh2), yBounds=(yLow2, yHigh2), zBounds=(zLow2, zHigh2)} = c2

contained:: Cube -> Cube -> Bool
contained = flip contains

findCompositeCubes :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> Cube -> Set Cube
findCompositeCubes allXBounds allYBounds allZBounds cube = Set.fromList [Cube{xBounds=xb, yBounds=yb, zBounds=zb} | xb<-containedXBounds, yb<-containedYBounds, zb<-containedZBounds]
    where
        containedXBounds = filter (isRangeContained (xBounds cube)) allXBounds
        containedYBounds = filter (isRangeContained (yBounds cube)) allYBounds
        containedZBounds = filter (isRangeContained (zBounds cube)) allZBounds
        isRangeContained (outerLow, outerHigh) (innerLow, innerHigh) = (outerLow <= innerLow) && (innerHigh <= outerHigh)

updateLights :: (Cube -> Set Cube) -> Set Cube -> Instruction -> Set Cube
updateLights splitCube litCubes instruction
    | isOn instruction = Set.union instructionCubes litCubes
    | otherwise        = Set.difference litCubes instructionCubes
    where
        instructionCubes = (splitCube . cube) instruction


main :: IO ()
main = do
    input <- readFile "app/input.txt"
    let instructions = (map (regularParse parseInstruction) . lines) input
    let cubes = map cube instructions
    let xCoords = (sort . Set.toList . Set.unions . map ((\(x1, x2) -> Set.fromList [x1, x2]) . xBounds)) cubes
    let yCoords = (sort . Set.toList . Set.unions . map ((\(y1, y2) -> Set.fromList [y1, y2]) . yBounds)) cubes
    let zCoords = (sort . Set.toList . Set.unions . map ((\(z1, z2) -> Set.fromList [z1, z2]) . zBounds)) cubes

    let allXBounds = zip xCoords (tail xCoords)
    let allYBounds = zip yCoords (tail yCoords)
    let allZBounds = zip zCoords (tail zCoords)
    let splitCube = findCompositeCubes allXBounds allYBounds allZBounds

    -- let finalLitCubes = foldl (updateLights splitCube) Set.empty instructions
    print $ (\l -> l !! 10) $ map (Set.size . splitCube) cubes
    {--
    print $ splitRanges (10, 15) (12, 100)
    print $ splitRanges (4, 10) (3, 6)
    print $ splitRanges (10, 15) (12, 100)
    print $ splitRanges (0, 10) (2, 5)
    print $ splitRanges (2, 5) (0, 10)
    print $ splitRanges (2, 5) (5, 10)
    print $ splitRanges (2, 5) (5, 6)
    --}