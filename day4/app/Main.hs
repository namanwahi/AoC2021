module Main where

import Data.List.Split
import Data.List

-- two dimensional structure containing the number and if it has been called
type Board = [[Int]]

-- parse data and return a list of bingo numbers and the bingo boards
parseData :: [String] -> ([Int], [Board])
parseData (bingoLine : "" : boardLines) = (bingoNums, boards)
    where
        bingoNums = map read (splitOn "," bingoLine)
        boardRowToNums = (map read) . (filter (/="")) . (splitOn " ")
        boards = map (map (boardRowToNums)) (splitOn [""] boardLines)
parseData _ = error "invalid format"

-- check if a bord has won given a list of the called numbers
hasWon :: Board -> [Int] -> Bool
hasWon board called = (anyRows board) || anyRows (transpose board)
    where
        completeRow = and . (map (\num -> elem num called))
        anyRows = or . (map completeRow)
        transpose ([]:_) = []
        transpose x = (map head x) : transpose (map tail x)

-- runs bingo given the list of boards, the called numbers. Returns the winning boards and the
-- called numbers up until them respectively.
playBingo :: ([Int], [Board]) -> [(Board, [Int])]
playBingo (toCall, boards) = playBingo' toCall [] [] []
    where
        playBingo' :: [Int] -> [Int] -> [(Board, [Int])] ->  [(Board, [Int])]
        playBingo' [] _ acc = acc
        playBingo' (nextToCall : toCall) haveCalled acc = case findWinner of Just winner -> playBingo' toCall (nextToCall : haveCalled) (acc ++ [(winner, haveCalled)])
                                                                             Nothing     -> playBingo' toCall (nextToCall : haveCalled) acc
            where
                oldWinners = map fst acc
                findWinner = find (\b -> (hasWon b haveCalled) && (not (elem b oldWinners))) boards


-- calculate score
score :: (Board, [Int]) -> Int
score (boards, haveCalled) = (head haveCalled) * (sum (map (sum . (filter (\n -> not (elem n haveCalled)))) boards))

main :: IO ()
main = do
    input <- readFile "app/input.txt"
    --print $ parseData $ lines input
    print $ map head$ map fst $ playBingo $ parseData $ lines input
    print $ head $ playBingo $ parseData $ lines input
    print $ head $ reverse $ playBingo $ parseData $ lines input

