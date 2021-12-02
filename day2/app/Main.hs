module Main where

-- moves coordinate by one instruction
move :: (Int, Int) -> String -> (Int, Int)
move (x, y) ('f':'o':'r':'w':'a':'r':'d':' ':num) = (x + (read num), y)
move (x, y) ('d':'o':'w':'n':' ':num) = (x, y + (read num))
move (x, y) ('u':'p':' ':num) = (x, y - (read num))

-- moves coordinate by list of instructions
travel :: [String] -> (Int, Int)
travel = foldl move (0, 0)

-- move coordinates by one instruction (with aim)
moveWithAim :: (Int, Int, Int) -> String -> (Int, Int, Int)
moveWithAim (x, y, aim) ('f':'o':'r':'w':'a':'r':'d':' ':num) = (x + (read num), y + (aim * read num), aim)
moveWithAim (x, y, aim) ('d':'o':'w':'n':' ':num) = (x, y, aim + (read num))
moveWithAim (x, y, aim) ('u':'p':' ':num) = (x, y, aim - (read num))

-- moves coordinate by list of instructions
travelWithAim :: [String] -> (Int, Int, Int)
travelWithAim = foldl moveWithAim (0, 0, 0)

main :: IO ()
main = do
    input <- readFile "app/input.txt"
    let instructions = lines input
    -- part 1
    let (x, y) = travel instructions
    print (x * y)
    -- part 2
    let (x, y, _) = travelWithAim instructions
    print (x * y)