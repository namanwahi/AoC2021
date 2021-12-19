module Main where
import Data.Maybe
import Data.List

-- GLOBAL assumptions
-- Trench is to the right and below start
-- Starting velocities are positive integers

getXPositions :: Int -> [Int]
getXPositions v = scanl (+) 0 ([v, v-1..0] ++ repeat 0)

getYPositions :: Int -> [Int]
getYPositions v = scanl (+) 0 [v, v-1..]

doesXFallInBounds :: (Int, Int) -> Int -> Bool
doesXFallInBounds (xMin, xMax) v = lastXPosition <= xMax && lastXPosition >= xMin
    where
        lastXPosition = (last . takeWhile (>= xMin) . getXPositions) v

doesYFallInBounds :: (Int, Int) -> Int -> Bool
doesYFallInBounds (yMin, yMax) v = lastYPosition <= yMax && lastYPosition >= yMin
    where
        lastYPosition = (last . takeWhile (>= yMin) . getYPositions) v

getHighestYPosition :: Int -> Int
getHighestYPosition v = (fst . fromJust . find (\(p, next) -> p == next)) (zip yPositions (tail yPositions))
    where
        yPositions = getYPositions v

launchProbe :: Int -> Int -> Int -> [(Int, Int)]
launchProbe xVel yVel = take 1000 $ zip (getXPositions xVel) (getYPositions yVel)

passThroughTrench :: (Int, Int, Int, Int) -> [(Int, Int)] -> Bool
passThroughTrench (xMin, xMax, yMin, yMax) points = any (\(x, y) -> x <= xMax && x >= xMin && y <= yMax && y >= yMin) points

main :: IO ()
main = do
    let bounds@(xMin, xMax, yMin, yMax) = (192,251,-89,-59)

    -- part 1
    let maxYVel = (0 - yMin) - 1
    print $ getHighestYPosition maxYVel

    -- part 2
    let candidateYVels = [(-maxYVel-1)..maxYVel]
    let candidateXVels = [1..xMax]
    print candidateYVels
    print candidateXVels
    print $ length $ filter (\(xVel, yVel) -> passThroughTrench bounds (launchProbe xVel yVel)) [ (xVel, yVel) | xVel<-candidateXVels, yVel<-candidateYVels]
