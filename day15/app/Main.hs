module Main where
import Debug.Trace (trace)
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
type RiskMap = Map Point Int

-- for dijkstra
-- distance from source
type Dist = Map Point Int
-- previous Point
type Prev = Map Point Point
-- point PQueue
type PointPQueue = MinPQueue Point Int

-- part 1
-- make riskMap from 2-d list. Heightmap is a map from a coordinate (x,y) to a height
makeRiskMap :: [[Int]] -> RiskMap
makeRiskMap grid = foldl (\acc (x, y) -> Map.insert (x, y) (grid !! y !! x) acc) Map.empty allCoords
    where
        allCoords = [(x, y) | x <- [0..((length . head) grid)-1], y <-[0..(length grid)-1]]


-- key a map where you know there the key exists
sureLookup :: Ord k => k -> Map k a -> a
sureLookup key map = fromJust $ Map.lookup key map

-- get neighbours of a point from the heightmap
getNeighbours :: RiskMap -> Point -> [Point]
getNeighbours riskMap (x, y) = filter (\p -> Map.member p riskMap) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]


updatePriority :: Point -> Int -> PointPQueue -> PointPQueue
updatePriority point priority pQueue
    | point `elem` (MinPQueue.keysU pQueue) = MinPQueue.mapWithKey (\p oldPriority -> if p == point then priority else oldPriority) pQueue
    | otherwise = MinPQueue.insert point priority pQueue

-- dijkstra path finding algorith,
dijkstra :: RiskMap -> Point -> Point -> Int
dijkstra riskMap start target = dijkstra' (MinPQueue.singleton (0, 0) 0) initialDist initialPrev
    where
        initialPrev = Map.empty
        initialDist = Map.unions $ (Map.singleton (0, 0) 0) : [Map.singleton p (maxBound::Int) | p <- nonSourceNodes]
        nonSourceNodes = filter (/=(0, 0)) $ Map.keys riskMap
        dijkstra' :: PointPQueue -> Dist -> Prev -> Int
        dijkstra' pQueue dist prev
            | u == target = distToU
            | otherwise = trace ("after updae:" ++ show (MinPQueue.toList pQueue'')) $ dijkstra' pQueue'' dist' prev'
            where
                ((u, _), pQueue') = trace ("before pop: " ++ show (MinPQueue.toList pQueue)) (MinPQueue.deleteFindMin pQueue)
                distToU = sureLookup u dist
                vs = getNeighbours riskMap u
                (pQueue'', dist', prev') = trace ("after pop: " ++ show (MinPQueue.toList pQueue') ++ " neighbours: " ++ (show vs)) $ foldr updateState (pQueue', dist, prev) vs
                updateState :: Point -> (PointPQueue, Dist, Prev) -> (PointPQueue, Dist, Prev)
                updateState v (pq, d, p)
                    | alt <= (sureLookup v riskMap) = (updatePriority v alt pq, Map.insert v alt d, Map.insert v u p)
                    | otherwise = (pq, d, p)
                    where
                        alt' = distToU + (sureLookup v riskMap)
                        alt = trace ("alt: " ++ show alt' ++ " distance to V " ++ show (sureLookup v riskMap)) alt'


main :: IO ()
main = do
    input <- readFile "app/input.txt"
    let grid = map (map digitToInt) $ lines input :: [[Int]]
    let riskMap = makeRiskMap grid
    print riskMap
    print $ getNeighbours riskMap (0,1)
    let x = 1 :: Int
    print $ MinPQueue.toList $ updatePriority (1,2) 0 $ updatePriority (2,3) 4 $ MinPQueue.singleton (1,2) x
    print $ dijkstra riskMap (0, 0) (9, 9)