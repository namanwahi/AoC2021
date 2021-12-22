module Main where
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace (trace)

data GameState = GameState {
    player:: Int,
    player1Pos :: Int,
    player2Pos :: Int,
    player1Score :: Int,
    player2Score :: Int
} deriving (Show, Eq, Ord)

-- map of form Gamestate -> (player1 wins, player2 wins)
type WinCache = Map GameState (Int, Int)

playRound :: Int -> GameState -> GameState
playRound roll (GameState {player=player, player1Pos=pos1, player2Pos=pos2, player1Score=score1, player2Score=score2})
    | player == 1 = GameState {player=2, player1Pos=newPos1, player2Pos=pos2, player1Score=score1 + newPos1, player2Score=score2}
    | player == 2 = GameState {player=1, player1Pos=pos1, player2Pos=newPos2, player1Score=score1, player2Score=score2 + newPos2}
    where
        movePosition pos steps = if ((pos + steps) `mod` 10) == 0 then 10 else (pos + steps) `mod` 10
        newPos1 = movePosition pos1 roll
        newPos2 = movePosition pos2 roll

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (x, y) (a, b) = (x + a, y + b)

countWins :: GameState -> WinCache -> (WinCache, (Int, Int))
countWins gameState cache
    | Map.member gameState cache = (cache, fromJust $ Map.lookup gameState cache)
    | player1Score gameState >= 21 = (Map.insert gameState (1, 0) cache, (1, 0))
    | player2Score gameState >= 21 = (Map.insert gameState (0, 1) cache, (0, 1))
    | otherwise                    = foldr addCountsUpdateCache (cache, (0, 0)) sumRolls
    where
        addCountsUpdateCache :: Int -> (WinCache, (Int, Int)) -> (WinCache, (Int, Int))
        addCountsUpdateCache sumRoll (accCache, accWinCounts) = (Map.insert nextGameState newWinCounts newCache, add accWinCounts newWinCounts)
            where
                nextGameState = playRound sumRoll gameState
                (newCache, newWinCounts) = countWins nextGameState accCache

        sumRolls = [x + y + z | x<-[1..3], y<-[1..3], z<-[1..3]]


main :: IO ()
main = do
    let rolls = [x + y + z | x<-[1..3], y<-[1..3], z<-[1..3]]
    let initialState = (GameState {player=1, player1Pos=4, player2Pos=6, player1Score=0, player2Score=0})
    print $ snd $ countWins initialState Map.empty