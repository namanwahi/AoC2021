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
        pointsToIncrease = filter (\p -> (not (p `elem` pointsToFlash)) && (fromJust $ Map.lookup p octopuses) /= 0) $ (concat . map getNeighbours) pointsToFlash
        resetFlashed = Map.mapWithKey (\point energy -> if point `elem` pointsToFlash then 0 else energy) octopuses

-- one step of the algorithm
step :: Map (Int, Int) Int -> Map (Int, Int) Int
step = resetEnergy . lastFlash . iterate flashOnce . incrementEnergy
    where
        incrementEnergy = Map.map (+1)
        lastFlash = fst . fromJust . find (\(curr, next) -> curr == next) . (\xs -> zip xs (tail xs))
        resetEnergy = Map.map (\n -> if n > 9 then 0 else n)

-- total flashes after n steps
flashesAfternSteps :: Int -> Map (Int, Int) Int -> Int
flashesAfternSteps n = sum . map countFlashes . take (n + 1) . iterate step
    where
        countFlashes = length . filter (\(p, energy) -> energy == 0) . Map.toList

-- when do all the octopuses flash
whenAllFlash :: Map (Int, Int) Int -> Int
whenAllFlash = fst . head . filter allFlashing . zip [0..] . iterate step
    where
        allFlashing = (\(_, octs) -> all (==0) $ map snd $ Map.toList octs)

main :: IO ()
main = do
    input <- readFile "app/input.txt"
    let grid = map (map digitToInt) $ lines input :: [[Int]]
    let octopuses = makeOctopusses grid
    -- print part 1
    print $ flashesAfternSteps 100 octopuses
    -- part 2
    print $ whenAllFlash octopuses