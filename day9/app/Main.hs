module Main where
import Data.Maybe
import Data.List (sort)
import Data.Map (Map)
import Data.Char(digitToInt)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- part 1
-- make heightmap from 2-d list. Heightmap is a map from a coordinate (x,y) to a height
makeHeightMap :: [[Int]] -> Map (Int, Int) Int
makeHeightMap grid = foldl (\acc (x, y) -> Map.insert (x, y) (grid !! y !! x) acc) Map.empty allCoords
    where
        allCoords = [(x, y) | x <- [0..((length . head) grid)-1], y <-[0..(length grid)-1]]

-- Find all the low points given a height map
findLowPoints :: Map (Int, Int) Int -> [(Int, Int)]
findLowPoints heightMap = (filter (\(x, y) -> all (>(sureLookUp (x, y))) (adjacentHeights (x, y)))) (Map.keys heightMap)
    where
        adjacentHeights (x, y) = map (\p -> Map.findWithDefault 10 p heightMap) [(x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)]
        sureLookUp point = fromJust $ (Map.lookup point heightMap)

-- get the total risk level
totalRiskLevel :: Map (Int, Int) Int -> Int
totalRiskLevel heightMap = (sum . (map (+1)) . (map sureLookUp) . findLowPoints) heightMap
    where
        sureLookUp point = fromJust $ (Map.lookup point heightMap)

-- part 2
-- find basin low point
findBasin :: Map (Int, Int) Int -> (Int, Int) -> Maybe (Int, Int)
findBasin heightMap point@(x, y)
    | currentHeight == 9 = Nothing
    | adjacentBasinPoints == [] = Just point -- at the basin
    | otherwise = findBasin heightMap (head adjacentBasinPoints) -- traverse down any path lower
    where
        sureLookUp point = fromJust $ (Map.lookup point heightMap)
        currentHeight = sureLookUp point
        adjacentPointsOnMap = filter (\p -> Map.member p heightMap) [(x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)]
        adjacentBasinPoints = filter (\p -> ((sureLookUp p) < currentHeight)) adjacentPointsOnMap

main :: IO ()
main = do
    input <- readFile "app/input.txt"
    let grid = map (map digitToInt) $ lines input :: [[Int]]
    let heightMap = makeHeightMap grid
    print $ totalRiskLevel heightMap
    let basins = map (findBasin heightMap) (Map.keys heightMap)
    let lowPointCounts = map (\point -> length (filter (==Just point) basins)) (findLowPoints heightMap)
    print $ product $ take 3 $ reverse $ sort lowPointCounts