module Main where

import Debug.Trace
import Data.Maybe
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

type Point = (Int, Int)

data Burrow = Burrow {
    positionMap :: Map Point Char
} deriving (Eq, Ord)

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

moveEnergy :: Move -> Burrow -> Int
moveEnergy (start, finish) burrow@(Burrow{positionMap=pmap}) = (distance start finish) * scaleFactor valAtStart
    where
        valAtStart = atPosition burrow start
        distance (x1, y1) (x2, y2) = (abs (x1 - x2)) + (abs (y1 - y2))
        scaleFactor 'A' = 1
        scaleFactor 'B' = 10
        scaleFactor 'C' = 100
        scaleFactor 'D' = 1000

atPosition :: Burrow -> Point -> Char
atPosition (Burrow{positionMap=pmap}) p
    | not (p `elem` allPositions) = error ("invalid lookup" ++ show p)
    | otherwise                   = Map.findWithDefault '.' p pmap

isOccupied :: Burrow -> Point -> Bool
isOccupied burr p = (atPosition burr p) /= '.'

isFree :: Burrow -> Point -> Bool
isFree burr p = (atPosition burr p) == '.'

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
    | y == -2 && (isOccupied burrow (x, -1)) = []
    -- already in the right place
    | y == -1 && (x == getDestinationRoomX positionVal && positionVal == positionBelowVal) = []
    | y == -2 && (x == getDestinationRoomX positionVal) = []
    | otherwise = (map (\x -> (start, (x, 0))). filter isNotBlocked) freeHallwayXs
    where
        positionVal = atPosition burrow start
        positionBelowVal = atPosition burrow (x, y-1)
        occupiedHallwayXs = (map fst. filter (\(_, y) -> y == 0) . Map.keys) pmap
        freeHallwayXs = (map fst validHallwayCoords) \\ occupiedHallwayXs
        isNotBlocked freeX
            | freeX > x = (null . filter (\occupiedX -> x < occupiedX && occupiedX < freeX)) occupiedHallwayXs
            | freeX < x = (null . filter (\occupiedX -> freeX < occupiedX && occupiedX < x)) occupiedHallwayXs

getDestinationRoomX :: Char -> Int
getDestinationRoomX 'A' = 2
getDestinationRoomX 'B' = 4
getDestinationRoomX 'C' = 6
getDestinationRoomX 'D' = 8


targetBurrow :: Burrow
targetBurrow = Burrow{positionMap=Map.fromList targetPositions}
    where
        targetPositions = [((2, -2), 'A'), ((2, -1), 'A'), ((4, -2), 'B'), ((4, -1), 'B'), ((6, -2), 'C'), ((6, -1), 'C'), ((8, -2), 'D'), ((8, -1), 'D')]

getRoomMove :: Point -> Burrow -> Maybe Move
getRoomMove start@(x, 0) burrow@(Burrow{positionMap=pmap})
    -- neither occupied
    | isNotBlocked && isFree burrow (newX, -2) && isFree burrow (newX, -1) = Just (start, (newX, -2))
    -- bottom occupied and same val
    | isNotBlocked && (atPosition burrow (newX, -2) == valAtStart) && isFree burrow (newX, -1) = Just (start, (newX, -1))
    where
        valAtStart = fromJust $ Map.lookup (x, 0) pmap
        newX = getDestinationRoomX valAtStart
        occupiedHallwayXs = (map fst. filter (\(_, y) -> y == 0) . Map.keys) pmap
        isNotBlocked
            | x < newX = (null . filter (\occupiedX -> x < occupiedX && occupiedX < newX)) occupiedHallwayXs
            | newX < x = (null . filter (\occupiedX -> newX < occupiedX && occupiedX < x)) occupiedHallwayXs
getRoomMove _ _ = Nothing

getBestSequence :: Burrow -> (Int, [Burrow])
getBestSequence = getBestSequence' (0, [])
    where
        getBestSequence' (totalEnergy, burrows) burrow@(Burrow{positionMap=pmap})
            | length burrows /= length (nub burrows) = error ("duplicate burrows:\n" ++ (show (reverse burrows)))
            | burrow == targetBurrow = (totalEnergy, burrows)
            | null allMoves          = (maxBound, burrows)
            | otherwise              = (minimum . map (\m -> let newBurrow = applyMove m burrow in getBestSequence' (totalEnergy + moveEnergy m burrow, newBurrow: burrows) newBurrow)) allMoves
            where
                hallwayMoves = (concat . map (\start -> getPossibleHallwayMoves start burrow). Map.keys) pmap
                downMoves = (catMaybes . map (\start -> getRoomMove start burrow) . Map.keys) pmap
                allMoves = downMoves ++ hallwayMoves


main :: IO ()
main = do
    print allPositions
    let startingPositions = [((2, -2), 'A'), ((2, -1), 'B'), ((4, -2), 'D'), ((4, -1), 'C'), ((6, -2), 'C'), ((6, -1), 'B'), ((8, -2), 'A'), ((8, -1), 'D')]
    let initialBurrow = Burrow{positionMap=Map.fromList startingPositions}
    print initialBurrow
    let moveApllied = applyMove ((2, -1), (5, 0)) initialBurrow
    --print moveApllied
    --print $ validHallwayCoords
    --print $ getPossibleHallwayMoves (2, -2) initialBurrow
   -- print $ getPossibleHallwayMoves (2, -1) initialBurrow
    --print $ getPossibleHallwayMoves (6, -1) moveApllied
    --print $ getPossibleHallwayMoves (2, -2) moveApllied
    print $ getBestSequence initialBurrow