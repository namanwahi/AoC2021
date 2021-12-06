module Main where

import Data.List.Split (splitOn)

-- Slow part 1
-- update a school of fish by one day
updateSchool :: [Int] -> [Int]
updateSchool school = (map (\n -> if n == 0 then 6 else n - 1) school) ++ (take (length (filter (==0) school)) (repeat 8))

-- simulate a school of fish over n days
simulateSchool :: [Int] -> Int -> [Int]
simulateSchool school days = last (take (days + 1) (iterate updateSchool school))

-- Fast Part 2

-- represent school as list of counts of the number of fish at each internal timer value
schoolCounts :: [Int] -> [Int]
schoolCounts school = map (\n -> length (filter (==n) school)) [0..8]

-- update counts by one day
updateSchoolCounts :: [Int] -> [Int]
updateSchoolCounts (zeros: ones: twos: threes: fours: fives: sixes: sevens: eights: []) = ones: twos: threes: fours: fives: sixes: sevens + zeros: eights: zeros: []

-- simulate the counts of school of fish over n days
simulateSchoolCounts :: [Int] -> Int -> [Int]
simulateSchoolCounts schoolCounts days = last (take (days + 1) (iterate updateSchoolCounts schoolCounts))

main :: IO ()
main = do
    input <- readFile "app/input.txt"
    let initialState = map read (splitOn "," input) :: [Int]
    print $ sum $ simulateSchoolCounts (schoolCounts initialState) 256


