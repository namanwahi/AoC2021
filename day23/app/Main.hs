module Main where

import Debug.Trace
import Data.Maybe
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as MinPQueue

type Point = (Int, Int)

data Burrow = Burrow {
    positionMap :: Map Point Char
} deriving (Eq, Ord)

type Move = (Point, Point)

instance Show Burrow where
    show burrow@(Burrow{positionMap=pmap}) = intercalate "\n" (["", horizontalPadding] ++ rows ++ [horizontalPadding])
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
roomCoords = (concat . map (\x -> [(x, -1), (x, -2), (x, -3), (x, -4)])) [2, 4, 6, 8]

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
    | (not . null) aboveOccupants = []
    -- already in the right place
    | isRoomCorrect = []
    | otherwise = (map (\x -> (start, (x, 0))). filter isNotBlocked) freeHallwayXs
    where
        -- can't move checks
        aboveOccupants = (filter (\c -> c /= '.') . map (\p -> atPosition burrow p) . (\ys -> zip (repeat x) ys)) [y+1..(-1)]
        belowOccupants = (filter (\c -> c /= '.') . map (\p -> atPosition burrow p) . (\ys -> zip (repeat x) ys)) [(-4)..y-1]

        val = atPosition burrow start
        isRoomCorrect = all (\c -> (getDestinationRoomX c) == x) (val: belowOccupants)

        -- can move
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
        targetPositions = (concat . map (\c -> zip (zip (repeat $ getDestinationRoomX c) [-4..(-1)]) (repeat c))) ['A', 'B', 'C', 'D']

getRoomMove :: Point -> Burrow -> Maybe Move
getRoomMove start@(x, 0) burrow@(Burrow{positionMap=pmap})
    | isNotBlockedInHallway && allCorrect = Just (start, (newX, -4 + length occupants))
    where
        valAtStart = fromJust $ Map.lookup (x, 0) pmap
        newX = getDestinationRoomX valAtStart

        occupants = (filter (/= '.') . map (\y -> atPosition burrow (newX, y))) [-4..(-1)]
        allCorrect = all (\c -> getDestinationRoomX c == newX) occupants

        occupiedHallwayXs = (map fst. filter (\(_, y) -> y == 0) . Map.keys) pmap
        isNotBlockedInHallway
            | x < newX = (null . filter (\occupiedX -> x < occupiedX && occupiedX < newX)) occupiedHallwayXs
            | newX < x = (null . filter (\occupiedX -> newX < occupiedX && occupiedX < x)) occupiedHallwayXs
getRoomMove _ _ = Nothing


-- key a map where you know there the key exists
sureLookup :: Ord k => k -> Map k a -> a
sureLookup key map = fromJust $ Map.lookup key map

getNeighbours :: Burrow -> [(Burrow, Int)]
--getNeighbours burrow = filter (\(b, _) -> b `elem` solutionBurrows) $ map (\m -> (applyMove m burrow, moveEnergy m burrow)) allMoves
getNeighbours burrow = map (\m -> (applyMove m burrow, moveEnergy m burrow)) allMoves
    where
        pmap = positionMap burrow
        hallwayMoves = (concat . map (\start -> getPossibleHallwayMoves start burrow). Map.keys) pmap
        downMoves = (catMaybes . map (\start -> getRoomMove start burrow) . Map.keys) pmap
        allMoves = downMoves ++ hallwayMoves

-- dijkstra path finding algorith,
getBestSequence :: Burrow -> (Int, Map Burrow Burrow)
getBestSequence initialBurrow@(Burrow{positionMap=pmap}) = dijkstra' (MinPQueue.singleton initialBurrow 0) (Map.singleton initialBurrow 0) (Map.empty) (Set.empty)
    where
        dijkstra' :: MinPQueue Burrow Int -> Map Burrow Int -> Map Burrow Burrow -> Set Burrow -> (Int, Map Burrow Burrow)
        dijkstra' pQueue dist prev visited
            | u == targetBurrow = (distToU, prev)
            | otherwise = dijkstra' pQueue'' dist' prev' visited'
            where
                -- pop burrow U from pQueue
                ((u, distToU), pQueue') = (MinPQueue.deleteFindMin pQueue)
                -- visiting u
                visited' = Set.insert u visited
                -- get next nodes (burrows and energy needed to move to them)
                vToEnergy = filter (\(v, _) -> not (Set.member v visited')) $ getNeighbours u
                (pQueue'', dist', prev') = foldr updateState (pQueue', dist, prev) vToEnergy

                updateState (v, energy) (pq, d, p)
                    | alt < (Map.findWithDefault (maxBound::Int) v d) = (updatePriority v alt pq, Map.insert v alt d, Map.insert v u p)
                    | otherwise = (pq, d, p)
                    where
                        alt = distToU + energy

                updatePriority burrow newPriority pq = MinPQueue.insert burrow newPriority (MinPQueue.filterWithKey (\b _ -> b /= burrow) pq)

simulateMoves :: Burrow -> Int -> [Move] -> [(Burrow, Int)]
simulateMoves _ _ [] = []
simulateMoves burrow totalEnergy (move: moves) = (newBurrow, totalEnergy + energy) : (simulateMoves newBurrow (totalEnergy + energy) moves)
    where
        nextToEnergy = getNeighbours burrow
        newBurrow = applyMove move burrow
        energy = fromJust $ lookup newBurrow nextToEnergy

solutionBurrows :: [Burrow]
solutionBurrows = map fst simulateSolution
    where
        startingPositions = [((2, -1), 'B'), ((2, -2), 'D'), ((2, -3), 'D'), ((2, -4), 'A'), ((4, -1), 'C'), ((4, -2), 'C'), ((4, -3), 'B'), ((4, -4), 'D'),  ((6, -1), 'B'), ((6, -2), 'B'), ((6, -3), 'A'), ((6, -4), 'C'),  ((8, -1), 'D'), ((8, -2), 'A'), ((8, -3), 'C'), ((8, -4), 'A')]
        initialBurrow = Burrow{positionMap=Map.fromList startingPositions}
        simulateSolution = simulateMoves initialBurrow 0 solutionMoves

reconstructPath :: Map Burrow Burrow -> Burrow -> [Burrow]
reconstructPath prevs burrow
    | Map.member burrow prevs = (reconstructPath prevs (fromJust $ Map.lookup burrow prevs)) ++ [burrow]
    | otherwise               = []

main :: IO ()
main = do
    print allPositions
    let startingPositions = [((2, -1), 'B'), ((2, -2), 'D'), ((2, -3), 'D'), ((2, -4), 'A'), ((4, -1), 'C'), ((4, -2), 'C'), ((4, -3), 'B'), ((4, -4), 'D'),  ((6, -1), 'B'), ((6, -2), 'B'), ((6, -3), 'A'), ((6, -4), 'C'),  ((8, -1), 'D'), ((8, -2), 'A'), ((8, -3), 'C'), ((8, -4), 'A')]
    --let startingPositions = [((2, -1), 'B'), ((2, -2), 'D'), ((2, -3), 'D'), ((2, -4), 'B'), ((4, -1), 'C'), ((4, -2), 'C'), ((4, -3), 'B'), ((4, -4), 'C'),  ((6, -1), 'A'), ((6, -2), 'B'), ((6, -3), 'A'), ((6, -4), 'D'),  ((8, -1), 'D'), ((8, -2), 'A'), ((8, -3), 'C'), ((8, -4), 'A')]
    --print targetBurrow
    let initialBurrow = Burrow{positionMap=Map.fromList startingPositions}
    print initialBurrow

    let simulateSolution = simulateMoves initialBurrow 0 solutionMoves :: [(Burrow, Int)]
    mapM_ print (map fst simulateSolution)
    print $ map snd simulateSolution
    print $ sum $ map snd simulateSolution
    let (score, prevs) = getBestSequence initialBurrow
    let burrows = reconstructPath prevs targetBurrow
    mapM_ print burrows
    print $ score

solutionMoves :: [Move]
solutionMoves =
    [((8, -1), (10, 0))
    ,((8, -2), (0, 0))
    ,((6, -1), (9, 0))
    ,((6, -2), (7, 0))
    ,((6, -3), (1, 0))
    ,((4, -1), (5, 0))
    ,((5, 0), (6, -3))
    ,((4, -2), (5, 0))
    ,((5, 0), (6, -2))
    ,((4, -3), (5, 0))
    ,((4, -4), (3, 0))
    ,((5, 0), (4, -4))
    ,((7, 0), (4, -3))
    ,((9, 0), (4, -2))
    ,((8, -3), (7, 0))
    ,((7, 0), (6, -1))
    ,((8, -4), (9, 0))
    ,((3, 0), (8, -4))
    ,((2, -1), (3, 0))
    ,((3, 0), (4, -1))
    ,((2, -2), (7, 0))
    ,((7, 0), (8, -3))
    ,((2, -3), (3, 0))
    ,((1, 0), (2, -3))
    ,((0, 0), (2, -2))
    ,((3, 0), (8, -2))
    ,((9, 0), (2, -1))
    ,((10, 0), (8, -1))]