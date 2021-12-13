module Main where

import Text.Parsec (parse, ParseError, many)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit, char, string, anyChar)
import Text.Parsec.Combinator (sepBy)

import Data.Either (fromRight)
import Data.Set (Set)
import qualified Data.Set as Set

-- (x, y) coordinates
type Point = (Int, Int)
-- (axis in {'x','y'}, position)
type Fold = (Char, Int)
-- set of Dots on the paper
type Dots = Set Point

-- parsing code
regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

parseInteger :: Parser Int
parseInteger = do { n <- many digit ; return (read n) }

parseDot :: Parser Point
parseDot = do { x1 <- parseInteger; char ','; y1 <- parseInteger; return (x1, y1) }

parseFold :: Parser Fold
parseFold = do { string "fold along " ; axis <- anyChar ; char '=' ; pos <- parseInteger ; return (axis, pos)}

-- given a fold instruction, maps a point to it's new location
flipPoint :: Fold -> Point -> Point
flipPoint ('x', flipPos) (x, y) = if x > flipPos then (flipPos - (x - flipPos), y) else (x, y)
flipPoint ('y', flipPos) (x, y) = if y > flipPos then (x, flipPos - (y - flipPos)) else (x, y)

-- print dots
showDots :: Dots -> String
showDots dots = (concat . map showRow) [0..maxY]
    where
        maxX = Set.foldr (\(x, _) w -> max x w) 0 dots
        maxY = Set.foldr (\(_, y) h -> max y h) 0 dots
        showRow y = (map (\x -> if Set.member (x, y) dots then '#' else ' ') [0..maxX]) ++ ['\n']

main :: IO ()
main = do
    input <- readFile "app/input.txt"
    let inputLines = lines input
    let dots = Set.fromList $ map (fromRight (-1, -1) . regularParse parseDot) $ takeWhile (/="") inputLines
    let folds = map (fromRight ('z', -1) . regularParse parseFold) $ (tail . dropWhile (/="")) inputLines
    -- part 1
    print $ length $ Set.map (flipPoint (head folds)) dots
    -- part 2
    let foldedDots = foldl (\dots fold -> Set.map (flipPoint fold) dots) dots folds
    writeFile "output.txt" (showDots foldedDots)

