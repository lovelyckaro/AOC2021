module Main where
import SantaLib
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Data.Graph.Inductive hiding (neighbors)
import Data.Graph.Inductive.Query.SP
import Control.Monad
import Data.Maybe
import Data.Foldable

pInp :: String -> Vector (Vector Int)
pInp inp = inp |> lines |> map (map (:[])) |> map (map read) |> map V.fromList |> V.fromList

type Point = (Int,Int)
type Cost = Int

(+|) :: Vector (Vector Int) -> Vector (Vector Int) -> Vector (Vector Int)
(+|) = V.zipWith (<>)

enlargenMatrix :: Vector (Vector Int) -> Vector (Vector Int)
enlargenMatrix tile = foldl' (<>) V.empty [row n | n <- [0..4]]
  where
    succ = V.map (V.map (\x -> if x + 1 > 9 then 1 else x + 1))
    tiles = iterate succ tile
    rows = length tile
    row n = foldl' (+|) (V.replicate rows V.empty) $ take 5 $ drop n tiles

toGraph :: Vector (Vector Int) -> Gr Point Cost
toGraph matrix = mkGraph nodes edges
  where
    (height, width) = (length matrix, length $ V.head matrix)
    ps = [(row, col) | row <- [0..height - 1], col <- [0..width - 1]]
    nodes = zip [0..] ps
    edges = do
      (startNode, point) <- nodes
      endPoint <- neighbors point
      guard (checkBounds matrix endPoint)
      let (row, col) = endPoint
      let edge = (startNode, toNode endPoint, matrix ! row ! col)
      return edge
    toNode :: Point -> Node
    toNode (row, col) = row * width + col

checkBounds :: Vector (Vector Int) -> Point -> Bool
checkBounds matrix (row,col) = row >= 0 && col >= 0 && row < length matrix && col < length (V.head matrix)

neighbors :: Point -> [Point]
neighbors (row, col) = [(row, col + 1), (row, col - 1), (row - 1, col), (row + 1, col)]

part1 :: String -> Int
part1 inp = graph |> spLength 0 finalNode |> fromMaybe (-1)
  where
    matrix = pInp inp
    graph = toGraph matrix
    finalNode = sum (length <$> matrix) - 1

part2 :: String -> Int
part2 inp = graph |> spLength 0 finalNode |> fromMaybe (-1)
  where
    matrix = enlargenMatrix $ pInp inp
    graph = toGraph matrix
    finalNode = sum (length <$> matrix) - 1

main :: IO ()
main = do
  inp <- getInput 15
  putAnswer 15 Part1 (part1 inp)
  putAnswer 15 Part2 (part2 inp)
