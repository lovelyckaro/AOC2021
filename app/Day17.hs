{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.List.Split
import Data.Maybe
import SantaLib

type Point = (Int, Int)

data Area = Area {xmin, xmax, ymin, ymax :: Int}

pInp :: String -> Area
pInp inp = Area {..}
  where
    [xmin, xmax] = inp |> drop 15 |> takeWhile (/= ',') |> splitOn ".." |> map read
    [ymin, ymax] = inp |> dropWhile (/= ',') |> drop 4 |> splitOn ".." |> map read

within :: Point -> Area -> Bool
(x, y) `within` Area {..} = x >= xmin && x <= xmax && y >= ymin && y <= ymax

trajectoryUntilArea :: Area -> (Int, Int) -> Maybe [Point]
trajectoryUntilArea area (xv, yv) = go (0, 0) (xv, yv)
  where
    go :: Point -> (Int, Int) -> Maybe [Point]
    go (x, y) (xv, yv)
      | x > xmax area || y < ymin area = Nothing
      | (x, y) `within` area = Just [(x, y)]
      | xv < 0 = ((x, y) :) <$> go (x + xv, y + yv) (xv + 1, yv - 1)
      | xv == 0 = ((x, y) :) <$> go (x, y + yv) (0, yv - 1)
      | otherwise = ((x, y) :) <$> go (x + xv, y + yv) (xv - 1, yv - 1)

findAll :: Area -> [(Int, Int)]
findAll area = possibleVelocities |> filter (isJust . trajectoryUntilArea area)
  where
    possibleVelocities = (,) <$> [0 .. xmax area] <*> [ymin area .. abs (ymin area)]

findBest :: Area -> Int
findBest area = highestPoint
  where
    possibleVelocities = (,) <$> [0 .. xmax area] <*> [ymin area .. abs (ymin area)]
    trajectories = possibleVelocities |> map (trajectoryUntilArea area) |> filter isJust |> map fromJust
    highestPoint = maximum $ map (maximum . map snd) trajectories

part1 :: String -> Int
part1 inp = inp |> pInp |> findBest

part2 :: String -> Int
part2 inp = inp |> pInp |> findAll |> length

main :: IO ()
main = do
  inp <- getInput 17
  putAnswer 17 Part1 (part1 inp)
  putAnswer 17 Part2 (part2 inp)
