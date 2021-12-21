module Main where
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

type EnhacementAlgorithm = [Int]
type Point = (Int, Int)
type LightPixels = Set Point


-- make set of light pixels (1s)
makeLightPixels :: [[Char]] -> LightPixels
makeLightPixels grid = Set.fromList [(x, y) | x <- [0..((length . head) grid)-1], y <-[0..(length grid)-1], (grid !! y !! x) == '#']

-- lookup to see if a pixel of light (1) or dark (0)
lookupPixel :: LightPixels -> Point -> Int
lookupPixel lightPixels p = if Set.member p lightPixels then 1 else 0

-- binary to decimal
binToDec :: [Int] -> Int
binToDec = sum . (zipWith (\i b -> b * (2 ^ i)) [0..]) . reverse

-- get neighbouring points in order needed to product a binary string from the enhancement algorithm
getPointNeighbours :: Point -> [Point]
getPointNeighbours (x, y) = [(x + xOffset, y + yOffset) | yOffset<-[-1..1], xOffset<-[-1..1]]

-- get ehanced pixel value (1 or 0)
getEnhancedPixelValue :: LightPixels -> EnhacementAlgorithm -> Point -> Int
getEnhancedPixelValue lightPixels enhancementAlgorithm = ((\n -> enhancementAlgorithm !! n) . binToDec . map (lookupPixel lightPixels) . getPointNeighbours)

-- get all points to consider for the application of the enhancement algorithm
getPointsToConsider :: LightPixels -> [Point]
getPointsToConsider = (nub . concat . map (getPointNeighbours) . Set.toList)

-- get bounds of the light pixels (minX, maxY, minY, maxY)
getBounds :: LightPixels -> (Int, Int, Int, Int)
getBounds lightPoints = (minimum xs, maximum xs, minimum ys, maximum ys)
    where
        xs = map fst (Set.toList lightPoints)
        ys = map snd (Set.toList lightPoints)

enhance :: EnhacementAlgorithm -> LightPixels -> LightPixels
enhance enhancementAlgorithm lightPixels = Set.fromList lightPoints
    where
        pointsToConsider = getPointsToConsider lightPixels
        lightPoints = filter (\p -> (getEnhancedPixelValue lightPixels enhancementAlgorithm p) == 1) pointsToConsider

main :: IO ()
main = do
    input <- readFile "app/test_input.txt"
    let (algString: "": imageGrid) = lines input
    let enhancementAlgorithm = map (\c -> if c == '#' then 1 else 0) algString :: EnhacementAlgorithm
    let lightPixels = makeLightPixels imageGrid :: LightPixels
    let enhanceOnce = enhance enhancementAlgorithm
    print $ getBounds lightPixels
    -- print $ Set.size $ (enhanceOnce . enhanceOnce) lightPixels