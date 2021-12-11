module Main where
import Data.Char(digitToInt)
import Data.Maybe
import Data.Map (Map)
import Data.List
import qualified Data.Map as Map

-- make octopuses from 2-d list. (x, y) -> energy level
makeOctopusses :: [[Int]] -> Map (Int, Int) Int
makeOctopusses grid = foldl (\acc (x, y) -> Map.insert (x, y) (grid !! y !! x) acc) Map.empty allCoords
    where
        allCoords = [(x, y) | x <- [0..((length . head) grid)-1], y <-[0..(length grid)-1]]

-- one flash iteration
flashOnce :: Map (Int, Int) Int -> Map (Int, Int) Int
flashOnce octopuses = foldl (\octs p -> Map.adjust (+1) p octs) resetFlashed pointsToIncrease
    where
        pointsToFlash = filter (\p -> (fromJust $ Map.lookup p octopuses) > 9) (Map.keys octopuses) :: [(Int, Int)]
        getNeighbours p@(x, y) = filter (\adj -> (adj /= p) && (Map.member adj octopuses)) [(x', y') | x'<-[x-1..x+1], y'<-[y-1..y+1]]
        pointsToIncrease = filter (\p -> (fromJust $ Map.lookup p octopuses) /= 0) $  filter (\p -> not (p `elem` pointsToFlash)) $ (concat . map getNeighbours) pointsToFlash
        resetFlashed = Map.mapWithKey (\point energy -> if point `elem` pointsToFlash then 0 else energy) octopuses

-- one step of the algorithm
step :: Map (Int, Int) Int -> Map (Int, Int) Int
step octopuses = resetEnergy lastFlashes
    where
        increasedOcts = Map.map (+1) octopuses -- increase energy level by 1
        flashes = iterate flashOnce increasedOcts -- apply flashing infinitely many times
        lastFlashes = fst $ fromJust $ find (\(curr, next) -> curr == next) (zip flashes (tail flashes)) -- find convergence
        resetEnergy = Map.map (\n -> if n > 9 then 0 else n) -- reset energy

-- total flashes after n steps
flashesAfternSteps :: Map (Int, Int) Int -> Int -> Int
flashesAfternSteps octopuses n = sum $ map countFlashes octopusesPerStep
    where
        octopusesPerStep = take (n + 1) $ iterate step octopuses :: [Map (Int, Int) Int]
        countFlashes :: Map (Int, Int) Int -> Int
        countFlashes = length . filter (\(p, energy) -> energy == 0) . Map.toList


main :: IO ()
main = do
    input <- readFile "app/input.txt"
    let grid = map (map digitToInt) $ lines input :: [[Int]]
    let octopuses = makeOctopusses grid
    -- print part 1
    print $ flashesAfternSteps octopuses 100
    -- part 2
    print $ fst $ head $ filter (\(idx, octs) -> all (==0) $ map snd $ Map.toList octs) $ zip [0..] $ iterate step octopuses