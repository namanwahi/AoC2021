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

validBounds :: (Int, Int) -> Bool
validBounds = uncurry (<=)

validCube :: Cube -> Bool
validCube c = all validBounds [xBounds c, yBounds c, zBounds c]

volume :: Cube -> Int
volume c
    | not (validCube c) = error (show c)
volume (Cube{xBounds=(xlo, xhi), yBounds=(ylo, yhi), zBounds=(zlo, zhi)}) = (1 + xhi - xlo) * (1 + yhi - ylo) * (1 + zhi - zlo)

overlap :: Cube -> Cube -> Maybe Cube
overlap c1 c2
    | xlo <= xhi && ylo <= yhi && zlo <= zhi = Just Cube{xBounds=(xlo, xhi), yBounds=(ylo, yhi), zBounds=(zlo, zhi)}
    | otherwise                              = Nothing
    where
        xlo = max xlo1 xlo2
        xhi = min xhi1 xhi2
        ylo = max ylo1 ylo2
        yhi = min yhi1 yhi2
        zlo = max zlo1 zlo2
        zhi = min zhi1 zhi2
        Cube{xBounds=(xlo1, xhi1), yBounds=(ylo1, yhi1), zBounds=(zlo1, zhi1)} = c1
        Cube{xBounds=(xlo2, xhi2), yBounds=(ylo2, yhi2), zBounds=(zlo2, zhi2)} = c2

numLit :: [Instruction] -> Int
numLit = sum . map (\i -> if isOn i then volume (cube i) else -1 * volume (cube i)) . (foldl updateExplicitInstructions [])
    where
        updateExplicitInstructions :: [Instruction] -> Instruction -> [Instruction]
        updateExplicitInstructions explInstrs instr
            | isOn instr = instr : (negatedOverlaps ++ explInstrs)
            | otherwise  = negatedOverlaps ++ explInstrs
            where
                negatedOverlaps = [ Instruction{cube=fromJust c, isOn=not (isOn e)} | e<-explInstrs, let c = overlap (cube e) (cube instr), isJust c ]


main :: IO ()
main = do
    input <- readFile "app/input.txt"
    let instructions = (map (regularParse parseInstruction) . lines) input
    print $ numLit instructions
