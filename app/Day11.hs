{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable
import Data.Set (Set)
import qualified Data.Set as S
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import SantaLib

type Flashes = Int

type Grid = Vector (Vector Int)

type GridM = State Grid

pInp :: String -> Grid
pInp str = str |> lines |> map (map (: [])) |> map (map read) |> map V.fromList |> V.fromList

-- Tick entire generation of flashes
bigTick :: GridM Flashes
bigTick = do
  modify (V.map (V.map (+ 1)))
  smallTick
  g <- get
  return (g |> foldr (<>) V.empty |> V.filter (== 0) |> V.length)

-- Tick a single flash
smallTick :: GridM ()
smallTick = do
  g <- get
  let flashes = filter (\(row, col) -> g ! row ! col > 9) (points g)
  if null flashes
    then return ()
    else mapM_ incNeighbors flashes >> smallTick

modPoint :: Grid -> (Int -> Int) -> (Int, Int) -> Grid
modPoint g f (row, col) = g V.// [(row, grow')]
  where
    value = g ! row ! col
    value' = f value
    grow = g ! row
    grow' = grow V.// [(col, value')]

incNeighbors :: (Int, Int) -> GridM ()
incNeighbors p = do
  g <- get
  let validNeighbors = filter (checkBounds g) (neighbors p)
  let g' = foldl' (\grid (row, col) -> if grid ! row ! col == 0 then grid else modPoint grid (+ 1) (row, col)) g validNeighbors
  put (modPoint g' (const 0) p)

checkBounds :: Grid -> (Int, Int) -> Bool
checkBounds g (row, col) = row >= 0 && col >= 0 && row < length g && col < length (V.head g)

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (row, col) = [(row + dr, col + dc) | dr <- [-1 .. 1], dc <- [-1 .. 1], not (dr == 0 && dc == 0)]

points :: Grid -> [(Int, Int)]
points g = [(row, col) | row <- [0 .. length g - 1], col <- [0 .. length (V.head g) - 1]]

part1 :: String -> Flashes
part1 inp = inp |> pInp |> evalState (replicateM 100 bigTick) |> sum

part2 inp = inp |> pInp |> evalState untilAllFlash

untilAllFlash :: GridM Int
untilAllFlash = go 0
  where
    go :: Int -> GridM Int
    go n = do
      flashes <- bigTick
      if flashes == 100
        then return (n + 1)
        else go (n + 1)

main :: IO ()
main = do
  inp <- getInput 11
  putAnswer 11 Part1 (part1 inp)
  putAnswer 11 Part2 (part2 inp)