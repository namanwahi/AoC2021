module Main where
import Data.List (sort)
import Data.List.Split (splitOn)

main :: IO ()
main = do
    input <- readFile "app/input.txt"
    let nums = map read (splitOn "," input) :: [Int]
    -- part 1 (median to optimize L1 distance)
    print $ sum $ map (\n -> abs (((sort nums) !! ((length nums) `div` 2)) - n)) nums
    -- part 2 (mean to optimize L1 + L2 distance in the integer domain)
    let optimalPos = (sum nums) `div` (length nums)
    print $ sum $ map (\n -> sum [1..abs (optimalPos - n)]) nums