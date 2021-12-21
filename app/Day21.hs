{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.State
import Data.List
import Data.Map (Map, (!), (!?))
import qualified Data.Map as M
import Data.Maybe
import SantaLib

data Dice = Dice {dice :: [Int], rollCounter :: Int}

instance Show Dice where
  show (Dice [] counter) = "Dice which has run out of rolls"
  show (Dice (d : rest) counter) = unlines ["Dice:", "next-roll: " <> show d, "counter: " <> show counter]

die :: Dice
die = Dice (concat $ repeat [1 .. 100]) 0

data GameState = GameState {player :: Player, p1score, p1pos, p2score, p2pos :: Int}
  deriving (Show, Eq, Ord)

add :: (Int, Int) -> (Int, Int) -> (Int, Int)
add (a, b) (c, d) = (a + c, b + d)

move :: Int -> Int -> Int
move position roll = ((position + roll - 1) `mod` 10) + 1

pInp :: String -> GameState
pInp str = GameState Player1 0 p1Pos 0 p2Pos
  where
    (l1, _ : l2) = break (== '\n') str
    p1Pos = l1 |> stripPrefix "Player 1 starting position: " |> fromJust |> read
    p2Pos = l2 |> stripPrefix "Player 2 starting position: " |> fromJust |> read

data Player = Player1 | Player2
  deriving (Show, Eq, Ord)

roll :: State Dice Int
roll = do
  d <- gets dice
  modify (\(Dice d c) -> Dice (drop 3 d) (c + 3))
  return $ sum $ take 3 d

play1 :: GameState -> State Dice GameState
play1 gs@GameState {..}
  | p1score >= 1000 = return gs
  | p2score >= 1000 = return gs
  | otherwise = do
    r <- roll
    case player of
      Player1 -> play1 $ GameState Player2 (p1score + move p1pos r) (move p1pos r) p2score p2pos
      Player2 -> play1 $ GameState Player1 p1score p1pos (p2score + move p2pos r) (move p2pos r)

part1 :: String -> Int
part1 inp = min p1score p2score * rollCounter
  where
    init = pInp inp
    (GameState {..}, Dice {..}) = runState (play1 init) die

play2 :: GameState -> State (Map GameState (Int, Int)) (Int, Int)
play2 gs@GameState {..}
  | p1score >= 21 = return (1, 0)
  | p2score >= 21 = return (0, 1)
  | otherwise = do
    table <- get
    case table !? gs of
      Just res -> return res
      Nothing -> do
        let rolls = map sum (replicateM 3 [1, 2, 3])
        let futures = case player of
              Player1 -> [GameState Player2 (p1score + move p1pos roll) (move p1pos roll) p2score p2pos | roll <- rolls]
              Player2 -> [GameState Player1 p1score p1pos (p2score + move p2pos roll) (move p2pos roll) | roll <- rolls]
        scores <- mapM play2 futures
        let score = foldl' add (0, 0) scores
        modify (M.insert gs score)
        return score

part2 :: String -> Int
part2 inp = max p1wins p2wins
  where
    init = pInp inp
    (p1wins, p2wins) = evalState (play2 init) M.empty

main :: IO ()
main = do
  inp <- getInput 21
  putAnswer 21 Part1 (part1 inp)
  putAnswer 21 Part2 (part2 inp)
