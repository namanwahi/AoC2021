module Main where

import Debug.Trace
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

type Point = (Int, Int)

data Burrow = Burrow {
    positionMap :: Map Point Char
}

type Move = (Point, Point)

instance Show Burrow where
    show burrow@(Burrow{positionMap=pmap}) = intercalate "\n" ([horizontalPadding] ++ rows ++ [horizontalPadding])
        where
            maxX = (maximum . map fst) allPositions
            minY = (minimum . map snd) allPositions
            showRow y = '#': (map (\x -> if (x, y) `elem` allPositions then atPosition burrow (x, y) else '#') [0..maxX]) ++ "#"
            rows = (reverse . map (showRow)) [minY..0]
            horizontalPadding = take (length (head rows)) (repeat '#')

applyMove :: Move -> Burrow -> Burrow
applyMove (start, end) burrow@(Burrow{positionMap=pmap})
    | not (start `elem` allPositions)  = error "invalid start"
    | not (end `elem` allPositions)    = error "invalid end"
    | valAtStart == '.'                = error "start not found"
    | valAtEnd /= '.'                  = error "value at end"
    | (snd end) == 0 && not (end `elem` validHallwayCoords) = error "invalid move into hallway"
    | otherwise                        = Burrow{positionMap=Map.insert end valAtStart (Map.delete start pmap)}
    where
        valAtStart = atPosition burrow start
        valAtEnd   = atPosition burrow end

moveEnergy :: Move -> Char -> Int
moveEnergy (start, finish) c = (distance start finish) * scaleFactor c
    where
        distance (x1, y1) (x2, y2) = (abs (x1 - x2)) + (abs (y1 - y2))
        scaleFactor 'A' = 1
        scaleFactor 'B' = 10
        scaleFactor 'C' = 100
        scaleFactor 'D' = 1000

atPosition :: Burrow -> Point -> Char
atPosition (Burrow{positionMap=pmap}) p
    | not (p `elem` allPositions) = error "invalid lookup"
    | otherwise                   = Map.findWithDefault '.' p pmap

roomCoords :: [Point]
roomCoords = (concat . map (\x -> [(x, -1), (x, -2)])) [2, 4, 6, 8]

validHallwayCoords :: [Point]
validHallwayCoords = filter (\(x, y) -> (not (x `elem` roomXCoords)) && y==0) allPositions
    where
        roomXCoords = map fst roomCoords

allPositions :: [Point]
allPositions = hallwayCoords ++ roomCoords
    where
        hallwayCoords = zip [0..10] (repeat 0)

getPossibleHallwayMoves :: Point -> Burrow -> [Move]
getPossibleHallwayMoves start@(x, y) burrow@(Burrow{positionMap=pmap})
    | not (Map.member (x, y) pmap) = error "invalid lookup"
    -- alreadt at the top
    | y == 0 = []
    -- blocked by something above
    | y == -2 && (atPosition burrow (x, -1) /= '.') = []
    | otherwise = (map (\x -> (start, (x, 0))). filter isNotBlocked) freeHallwayXs
    where
        occupiedHallwayXs = (map fst. filter (\(_, y) -> y == 0) . Map.keys) pmap
        freeHallwayXs = (map fst validHallwayCoords) \\ occupiedHallwayXs
        isNotBlocked freeX
            | freeX > x = (null . filter (\occupiedX -> x < occupiedX && occupiedX < freeX)) occupiedHallwayXs
            | freeX < x = (null . filter (\occupiedX -> freeX < occupiedX && occupiedX < x)) occupiedHallwayXs



main :: IO ()
main = do
    print allPositions
    let startingPositions = [((2, -2), 'A'), ((2, -1), 'B'), ((4, -2), 'D'), ((4, -1), 'C'), ((6, -2), 'C'), ((6, -1), 'B'), ((8, -2), 'A'), ((8, -1), 'D')]
    let targetPositions = [((2, -2), 'A'), ((2, -1), 'A'), ((4, -2), 'B'), ((4, -1), 'B'), ((6, -2), 'C'), ((6, -1), 'C'), ((8, -2), 'D'), ((8, -1), 'D')]
    let initialBurrow = Burrow{positionMap=Map.fromList startingPositions}
    let targetBurrow = Burrow{positionMap=Map.fromList targetPositions}
    print initialBurrow
    let moveApllied = applyMove ((2, -1), (5, 0)) initialBurrow
    print moveApllied
    print $ validHallwayCoords
    print $ getPossibleHallwayMoves (2, -1) initialBurrow
    print $ getPossibleHallwayMoves (6, -1) moveApllied
    print $ getPossibleHallwayMoves (2, -2) moveApllied