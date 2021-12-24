module Main where

import Debug.Trace
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

type Point = (Int, Int)

data Burrow = Burrow {
    positionMap :: Map Point Char
}

atPosition :: Burrow -> Point -> Char
atPosition (Burrow{positionMap=pmap}) p
    | not (p `elem` allPositions) = error "invalid lookup"
    | otherwise                   = Map.findWithDefault '.' p pmap

instance Show Burrow where
    show burrow@(Burrow{positionMap=pmap}) = intercalate "\n" ([horizontalPadding] ++ rows ++ [horizontalPadding])
        where
            maxX = (maximum . map fst) allPositions
            minY = (minimum . map snd) allPositions
            showRow y = '#': (map (\x -> if (x, y) `elem` allPositions then atPosition burrow (x, y) else '#') [0..maxX]) ++ "#"
            rows = (reverse . map (showRow)) [minY..0]
            horizontalPadding = take (length (head rows)) (repeat '#')

allPositions :: [Point]
allPositions = hallwayCoords ++ roomCoords
    where
        hallwayCoords = zip [0..10] (repeat 0)
        roomCoords = (concat . map (\x -> [(x, -1), (x, -2)])) [2, 4, 6, 8]


main :: IO ()
main = do
    print allPositions
    let startingPositions = [((2, -2), 'A'), ((2, -1), 'B'), ((4, -2), 'D'), ((4, -1), 'C'), ((6, -2), 'C'), ((6, -1), 'B'), ((8, -2), 'A'), ((8, -1), 'D')]
    let targetPositions = [((2, -2), 'A'), ((2, -1), 'B'), ((4, -2), 'D'), ((4, -1), 'C'), ((6, -2), 'C'), ((6, -1), 'B'), ((8, -2), 'A'), ((8, -1), 'D')]
    let initialBurrow = Burrow{positionMap=Map.fromList startingPositions}
    print initialBurrow
