module Main where
import Data.Char(digitToInt)

-- Part 1
gammaRate :: [[Int]] -> [Int]
gammaRate = (map (\n -> fromEnum (n >= 0))) . (foldl (zipWith (\dif bit -> if bit == 1 then dif + 1 else dif - 1)) (repeat 0))

flipBits :: [Int] -> [Int]
flipBits = map (1 -)

binToDec :: [Int] -> Int
binToDec = sum . (zipWith (\i b -> b * (2 ^ i)) [0..]) . reverse

-- Part 2
oxygenRating :: [[Int]] -> [Int]
oxygenRating codes = oxygenRating' codes []
    where
        oxygenRating' [remainingCode] acc = acc ++ remainingCode
        oxygenRating' codes acc = oxygenRating' remainingCodes (acc ++ [majorityBit])
            where
                remainingCodes = map tail (filter (\code -> (head code) == majorityBit) codes)
                countDifs = sum (map (\bit -> if bit == 1 then 1 else -1) (map head codes))
                majorityBit = fromEnum (countDifs >= 0)

co2ScrubberRating :: [[Int]] -> [Int]
co2ScrubberRating codes = co2ScrubberRating' codes []
    where
        co2ScrubberRating' [remainingCode] acc = acc ++ remainingCode
        co2ScrubberRating' codes acc = co2ScrubberRating' remainingCodes (acc ++ [minorityBit])
            where
                remainingCodes = map tail (filter (\code -> (head code) == minorityBit) codes)
                countDifs = sum (map (\bit -> if bit == 1 then 1 else -1) (map head codes))
                minorityBit = fromEnum (not (countDifs >= 0))

main :: IO ()
main = do
    input <- readFile "app/input.txt"
    let codes = map (map digitToInt) $ lines input

    -- part 1
    let g = gammaRate codes
    let e = flipBits g
    print $ (binToDec g) * (binToDec e)

    -- part 2
    print $ (binToDec (oxygenRating codes)) * (binToDec (co2ScrubberRating codes))

