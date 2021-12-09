{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import SantaLib
import Data.Vector (Vector, (!?), (!))
import qualified Data.Vector as V
import Data.Set (Set)
import qualified Data.Set as S
import Data.List

pInp :: String -> Vector (Vector Int)
pInp = V.fromList . map (V.fromList . map (read . (:[]))) . lines

isLowPoint :: Vector (Vector Int) -> (Int, Int) -> Bool
isLowPoint vs (row, col) = length (filter (> this) neighbors) == neighborAmount
  where
    this = vs ! row ! col
    neighbors = [vs ! r ! c | (r,c) <- possibleNeighbors, checkBounds vs (r,c)]
    possibleNeighbors = [(row, col + 1), (row, col - 1), (row + 1, col), (row - 1, col)]
    neighborAmount = length neighbors

checkBounds :: Vector (Vector a) -> (Int, Int) -> Bool
checkBounds matrix (row, col) = row >= 0 && col >= 0 && col < width && row < height 
  where (height, width) = (length matrix, length (V.head matrix))

determineBasin :: Vector (Vector Int) -> (Int, Int) -> [(Int, Int)]
determineBasin matrix = connected neighbors
  where
    neighbors (row, col) = filter valid [(row, col + 1), (row, col - 1), (row + 1, col), (row - 1, col)]
      where valid (r,c) = checkBounds matrix (r,c)
                && matrix ! r ! c /= 9
                && matrix ! r ! c > matrix ! row ! col

badness :: Vector (Vector Int) -> (Int, Int) -> Int
badness  vs (row, col) = vs ! row ! col + 1

points :: Vector (Vector Int) -> [(Int,Int)]
points vs = [(row, col) | row <- [0.. length vs - 1], col <- [0.. length (V.head vs) - 1]]

part1 :: String -> Int
part1 s = sum badnesses
  where
    matrix = pInp s
    ps = points matrix
    lowPoints = filter (isLowPoint matrix) ps
    badnesses = map (badness matrix) lowPoints

part2 :: String -> Int
part2 s = sortedBasins |> take 3 |> map length |> product
  where
    matrix = pInp s
    ps = points matrix
    lowPoints = filter (isLowPoint matrix) ps
    basins = map (determineBasin matrix) lowPoints
    sortedBasins = sortBy (\a b -> compare (length b) (length a)) basins

main :: IO ()
main = do
  inp <- getInput 9
  ex <- getExample 9
  putAnswer 9 Part1 (part1 inp) 
  putAnswer 9 Part2 (part2 inp)