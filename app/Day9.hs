module Main where
import SantaLib
import Data.Vector (Vector, (!?), (!))
import qualified Data.Vector as V
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.List

pInp :: String -> Vector (Vector Int)
pInp = V.fromList . map (V.fromList . map (read . (:[]))) . lines

isLowPoint :: Vector (Vector Int) -> (Int, Int) -> Bool
isLowPoint vs (row, col) = length (filter (> this) neighbors) == neighborAmount
  where
    this = vs ! row ! col
    neighbors = catMaybes
      [ vs !? (row + 1) >>= (!? col)
      , vs !? (row - 1) >>= (!? col)
      , vs !? row >>= (!? (col - 1))
      , vs !? row >>= (!? (col + 1))]
    neighborAmount = length neighbors

determineBasin :: Vector (Vector Int) -> (Int, Int) -> Set (Int, Int)
determineBasin vs lowPoint = bfsUntil9 vs (V.singleton lowPoint) S.empty 
 
bfsUntil9 :: Vector (Vector Int) -> Vector (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
bfsUntil9 matrix queue visited 
  | V.null queue = visited
  | otherwise = bfsUntil9 matrix queue' visited'
    where 
      nextPoint@(row,col) = V.head queue
      neighbors = V.fromList [(row + 1, col), (row - 1, col), (row, col - 1), (row, col + 1)]
      queue' = V.tail queue <> V.filter addCondition neighbors
      addCondition p@(r, c) = not (S.member p visited) -- dont add points we've visited 
                          && r >= 0 -- bounds checking top
                          && c >= 0 -- bounds checking right
                          && r < length matrix -- bounds checking bottom
                          && c < length (V.head matrix) -- bounds checking left
                          && matrix ! r ! c /= 9 -- 9s are walls
                          && matrix ! r ! c > matrix ! row ! col -- lower points don't flow into higher points
      visited' = S.insert nextPoint visited
      

badness :: Vector (Vector Int) -> (Int, Int) -> Int
badness  vs (row, col) = vs ! row ! col + 1

points :: Vector (Vector Int) -> [(Int,Int)]
points vs = [(row, col) | row <- [0.. length vs - 1], col <- [0.. length (V.head vs) - 1]]

part1 :: String -> Int
part1 s = sum badnesses
  where
    matrix = pInp s
    ps = points matrix
    lowPoints = filter (isLowPoint matrix) ps
    badnesses = map (badness matrix) lowPoints

part2 :: String -> Int
part2 s = product $ map S.size $ take 3 sortedBasins
  where
    matrix = pInp s
    ps = points matrix
    lowPoints = filter (isLowPoint matrix) ps
    basins = map (determineBasin matrix) lowPoints
    sortedBasins = sortBy (\a b -> compare (S.size b) (S.size a)) basins

main :: IO ()
main = do
  inp <- getInput 9
  ex <- getExample 9
  putAnswer 9 Part1 (show $ part1 inp) 
  putAnswer 9 Part2 (show $ part2 inp)
