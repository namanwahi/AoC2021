module Main where

-- part 1 solution
numIncreases :: [Int] -> Int
numIncreases nums@(_: rest) = length (filter id (zipWith (<) nums rest))

-- part 2 solution
windowNumIncreases:: [Int] -> Int
windowNumIncreases as@(a : bs@(b : cs@(c : rest))) = numIncreases (zipWith3 (\x y z -> x + y + z) as bs cs)

main :: IO ()
main = do
    input <- readFile "app/input.txt"
    let nums = map (read::String->Int) $ lines input
    print (numIncreases nums)
    print (windowNumIncreases nums)