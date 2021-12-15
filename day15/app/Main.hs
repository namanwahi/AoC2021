module Main where
import Debug.Trace (trace, traceShow)
import Data.Maybe
import Data.List (sort)
import Data.Map (Map)
import Data.Char(digitToInt)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as MinPQueue

type Point =  (Int, Int)
data RiskMap = RiskMap (Map Point Int) Int Int deriving (Show)

-- for dijkstra
-- Maps point from source to distance
type Dist = Map Point Int
-- point min priority queue
type PointPQueue = MinPQueue Point Int

-- make riskMap from 2-d list. Heightmap is a map from a coordinate (x,y) to a height
makeRiskMap :: [[Int]] -> RiskMap
makeRiskMap grid = RiskMap map maxX maxY
    where
        map = foldl (\acc (x, y) -> Map.insert (x, y) (grid !! y !! x) acc) Map.empty allCoords
        maxX = (5 * length (head grid)) - 1
        maxY = (5 * length grid) - 1
        allCoords = [(x, y) | x <- [0..((length . head) grid)-1], y <-[0..(length grid)-1]]

-- key a map where you know there the key exists
sureLookup :: Ord k => k -> Map k a -> a
sureLookup key map = fromJust $ Map.lookup key map

-- get neighbours of a point from the heightmap
getNeighbours :: RiskMap -> Point -> [Point]
getNeighbours (RiskMap _ maxX maxY) (x, y) = filter inRange [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
    where
        inRange (x, y) = x >= 0 && x <= maxX && y >= 0 && y <= maxY

-- lookup riskt from risk map
lookupRisk :: Point -> RiskMap -> Int
lookupRisk (x, y) (RiskMap coordToRisk maxX maxY) = last $ take (xQuot + yQuot + 1) $ iterate incrementRisk $ sureLookup (correspondingXCoord, correspondingYCoord) coordToRisk
    where
        incrementRisk n = if n == 9 then 1 else n + 1
        originalScannedWidth = (maxX + 1) `div` 5
        originalScannedHeight = (maxY + 1) `div` 5
        (xQuot, correspondingXCoord) = (x `quotRem` originalScannedWidth)
        (yQuot, correspondingYCoord) = (y `quotRem` originalScannedHeight)

-- dijkstra path finding algorith,
dijkstra :: RiskMap -> Point -> Point -> Int
dijkstra riskMap@(RiskMap coordToRisk maxX maxY) start target = dijkstra' (MinPQueue.singleton (0, 0) 0) initialDist
    where
        initialDist = Map.unions $ (Map.singleton (0, 0) 0) : [Map.singleton p (maxBound::Int) | p <- nonSourceNodes]
        nonSourceNodes = filter (/=(0, 0)) $ [(x, y) | x<-[0..maxX], y<-[0..maxY]]
        dijkstra' pQueue dist
            | u == target = distToU
            | otherwise = dijkstra' pQueue'' dist'
            where
                ((u, _), pQueue') = (MinPQueue.deleteFindMin pQueue)
                distToU = sureLookup u dist
                vs = getNeighbours riskMap u
                (pQueue'', dist') = foldr updateState (pQueue', dist) vs
                updatePriority point priority pQueue
                    | point `elem` (MinPQueue.keysU pQueue) = MinPQueue.mapWithKey (\p oldPriority -> if p == point then priority else oldPriority) pQueue
                    | otherwise = MinPQueue.insert point priority pQueue
                updateState v (pq, d)
                    | alt < (sureLookup v d) = (updatePriority v alt pq, Map.insert v alt d)
                    | otherwise = (pq, d)
                    where
                        alt = distToU + (lookupRisk v riskMap)

main :: IO ()
main = do
    input <- readFile "app/input.txt"
    let grid = map (map digitToInt) $ lines input :: [[Int]]
    let riskMap@(RiskMap coordToRisk maxX maxY) = makeRiskMap grid
    print $ dijkstra riskMap (0, 0) (maxX, maxY)