module Main where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

type Point = (Int, Int)
type Herd = (Map Point Char, Int, Int)

makeHerd :: [[Char]] -> Herd
makeHerd grid = (map, maxX, maxY)
    where
        maxY = (length grid) - 1
        maxX = (length (head grid)) - 1
        map = Map.filter (/='.') $ foldl (\acc (x, y) -> Map.insert (x, y) (grid !! y !! x) acc) Map.empty allCoords
        allCoords = [(x, y) | x <- [0..((length . head) grid)-1], y <-[0..(length grid)-1]]

atPoint :: Point -> Herd -> Char
atPoint p (hmap, _, _) = Map.findWithDefault '.' p hmap

getNextEastPoint :: Herd -> Point -> Point
getNextEastPoint (hmap, maxX, _) (x, y) = ((x + 1) `mod` (maxX + 1),  y)

getNextSouthPoint :: Herd -> Point -> Point
getNextSouthPoint (hmap, _, maxY) (x, y) = (x,  (y + 1) `mod` (maxY + 1))

showHerd :: Herd -> String
showHerd herd@(hmap, maxX, maxY) = (concat . map showRow) [0..maxY]
    where
        showRow y = (map (\x -> atPoint (x, y) herd) [0..maxX]) ++ "\n"

moveEastFacing :: Herd -> Herd
moveEastFacing herd@(hmap, maxX, maxY) = (newHmap, maxX, maxY)
    where
        eastFacingPoints = (Map.keys . Map.filter (=='>')) hmap ::[Point]
        newEastFacingPoints = map movePointEast eastFacingPoints ::[Point]
        movePointEast p = if Map.member newPoint hmap then p else newPoint
            where
                newPoint = getNextEastPoint herd p

        newHmapSouth = Map.filter (=='v') hmap :: Map Point Char
        newHmapEast = Map.fromList (zip newEastFacingPoints (repeat '>')) :: Map Point Char
        newHmap = Map.union newHmapEast newHmapSouth :: Map Point Char

moveSouthFacing :: Herd -> Herd
moveSouthFacing herd@(hmap, maxX, maxY) = (newHmap, maxX, maxY)
    where
        southFacingPoints = (Map.keys . Map.filter (=='v')) hmap :: [Point]
        newSouthFacingPoints = map movePointSouth southFacingPoints :: [Point]
        movePointSouth p = if Map.member newPoint hmap then p else newPoint
            where
                newPoint = getNextSouthPoint herd p

        newHmapEast = Map.filter (=='>') hmap :: Map Point Char
        newHmapSouth = Map.fromList (zip newSouthFacingPoints (repeat 'v')) :: Map Point Char
        newHmap = Map.union newHmapEast newHmapSouth :: Map Point Char

step :: Herd -> Herd
step = moveSouthFacing . moveEastFacing

main :: IO ()
main = do
    input <- readFile "app/input.txt"
    let grid = lines input
    let herd = makeHerd grid
    print herd
    putStrLn $ showHerd herd
    let stepped = ((+1) . length . takeWhile (\(h, h') -> h /= h') . (\hs -> zip hs (tail hs)) . iterate step) herd
    print stepped
