module Main where
import Data.List.Split

type GameState = (Int, [Int], Int, Int, Int, Int, Int)

getScores :: GameState -> (Int, Int)
getScores (_, _, _, _, _, score1, score2) = (score1, score2)

isFinished :: GameState -> Bool
isFinished = (\(s1, s2) -> (s1 >= 1000) || (s2 >= 1000)) . getScores

getDiceRolls :: GameState -> Int
getDiceRolls (toPlay, (roll1: roll2: roll3: rest), rollCount, pos1, pos2, score1, score2) = rollCount

playRound :: GameState -> GameState
playRound (toPlay, (roll1: roll2: roll3: rest), rollCount, pos1, pos2, score1, score2)
    | toPlay == 1 = (2, rest, rollCount + 3, newPos1, pos2, score1 + newPos1, score2)
    | toPlay == 2 = (1, rest, rollCount + 3, pos1, newPos2, score1, score2 + newPos2)
    where
        sumRolls = roll1 + roll2 + roll3
        movePosition pos steps = if ((pos + steps) `mod` 10) == 0 then 10 else (pos + steps) `mod` 10

        newPos1 = movePosition pos1 sumRolls
        newPos2 = movePosition pos2 sumRolls

main :: IO ()
main = do
    let diceRolls = cycle [1..100]

    let finalState = head $ filter isFinished $ iterate playRound (1, diceRolls, 0, 4, 6, 0, 0)
    let rolls = getDiceRolls finalState
    let loserScore = (uncurry min) (getScores finalState)
    print $ getScores finalState
    print $ getDiceRolls finalState
    print (rolls * loserScore)
