{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad
import Data.Bifunctor
import Data.Foldable
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import SantaLib

pInp :: String -> [(Int, [Point])]
pInp inp = withPoints
  where
    scanners = splitOn "\n\n" inp
    titles = map pTitle scanners
    withPoints = map (second pPoints) titles

pTitle :: String -> (Int, String)
pTitle s = (number, tail rest)
  where
    (title, rest) = break (== '\n') s
    Just withoutPrefix = stripPrefix "--- scanner " title
    number = withoutPrefix |> takeWhile (/= ' ') |> read

pPoints :: String -> [Point]
pPoints s = ls
  where
    p line = let [x, y, z] = splitOn "," line in (read x, read y, read z)
    ls :: [Point]
    ls = map p $ lines s

type Point = (Int, Int, Int)

data Axis = X | Y | Z
  deriving (Show)

rotate :: Axis -> Point -> Point
rotate X (x, y, z) = (x, - z, y)
rotate Y (x, y, z) = (z, y, - x)
rotate Z (x, y, z) = (y, - x, z)

-- Rotations stolen, without shame from the internet
-- Credit: https://topaz.github.io/paste/#XQAAAQAjCgAAAAAAAAA2m8ixrhLu7YJFrd2FLde+PAG1Aui2yN36LC93WIQ2APMCiCS1T4jLaxB8BnzLyZh7YZaajuENzaVu+bqMEtO3HEhO5jhgZmq8VlGQHh/cTEnWKFEbnxRh/XYDWZiuA8smC7xPdnLN0pUjDdlKjg56yjgnmbZUYiG7Dp6P0BEfPf8dslfKGB7EjysInzgP8sofG+vusLYeUHCZmCdp/AQh7LZrueWiTM5nW7GdlzoDz0g44IpQcB1RCXj17cSSecpF4OwoPhEAWb/sUpW5lnZXb7DNL1mP6RCVpDl5twUVB15VlTOoUxjDwTjSiwjS9y8+znGkMvqYEfH5stXy1KYLBIvkarqjh5/XxHbxRGTrkp3u3UpkqHS0aT8loOqLowL1IFM5xoTbmlF7fQHt86wwxpJ5GMNfgtNWCeLDKX89ARjSWfeHbP8tbHnsTJZZIrVtkpjkupHmqPk6bMnjJo7MxKOouMJszOEx0fbzpYtbQHnfHutZWTMGJvEUT8IpBbPrE7H71eYpuViAmUZaGXC6Fd3O6NMgDvZDbIixZ8tbHGC3O4fcDkE8Tvj+tINDLLGg48j0VHraEJkriWXAnPhCyGdDHmz0kYkWtdzTdUtJTFWQbAB4SZJnktjVcFbjvwoGjej1pU8B1MIPNVMhkvmqt7f1GdFh1S0JbznYBxOjkduOWsJp5DqcKtq8ke7jQWaCVmEIZnIa7SLJbb3k4P//sX3ShVVZz29CnW9SPMZSYvsNL0LZNygAM2J2NbRUIdgr/lXsIqVg+T31Qj+xNoO2z3kJ10RBscdHSDcDuR2ztWM1TsEDKqwR1np2X3XcDkslkIoSyZ76n1c8hr003OxtX0wZjRSRmnYvEaOHi5gpDQJjBnvgnbeEtdnTQSqkXJGst3jAJ5PxBFSf+2M0qqYbzaI70+C+MMoR8JyrvR6oHwTwR33uR2QaBV4IvWrY+7HzyKP+QCTr2Jo7VnCBE+Up0kNShxudrs6XJ2ypaxLKpYYUKs9JgZTLGklqK0ggi12RaXhgFiWJxSBhh8CvC+54mdyQK0jYkgFppEk5qsAoael1PnGhH+TnhU+xufDLmAoagyH5gPST6GXjVwYrm4BJtNSm3aKsuUpiVrh15Vx5xUTaVo+QSH+CLqFGVOUwHl4PYlKzWBWj3alpcGln8FWZ4ZYcuUCbeXQH7rq2rVS3gAzXH59cdXhYiP4Tc9xF5H4BIoHfkfVfJR9EUQQtRmxvcxG+glWhPeh55B/sWFPOK1or9Qqvzq4X8Ra4s5eagyb8Z2cMV4+JXGUNsCN/w4P4BaI4Ou5NDno8uPY5unnBWqeO9M6vdNm4a0haT//Xk86G
rotations =
  [ [],
    [X],
    [Y],
    [Z],
    [X, X],
    [X, Y],
    [X, Z],
    [Y, X],
    [Y, Y],
    [Z, Y],
    [Z, Z],
    [X, X, X],
    [X, X, Y],
    [X, X, Z],
    [X, Y, X],
    [X, Y, Y],
    [X, Z, Z],
    [Y, X, X],
    [Y, Y, Y],
    [Z, Z, Z],
    [X, X, X, Y],
    [X, X, Y, X],
    [X, Y, X, X],
    [X, Y, Y, Y]
  ]

possibleOrientations :: [Point] -> [[Point]]
possibleOrientations points = do
  rots <- rotations
  return $ map (performRotations rots) points

performRotations :: [Axis] -> Point -> Point
performRotations rots p = foldl' (flip rotate) p rots

minus :: Point -> Point -> Point
minus (a, b, c) (d, e, f) = (a - d, b - e, c - f)

plus :: Point -> Point -> Point
plus (a, b, c) (d, e, f) = (a + d, b + e, c + f)

determineOne :: [Point] -> [Point] -> (Set Point, Point) -- (Set Point, Point)
determineOne known unkown = points
  where
    prod = liftM2 minus known
    makeFreqs :: [Point] -> Map Point Int
    makeFreqs = foldl' (\freqmap diff -> M.insertWith (+) diff 1 freqmap) M.empty
    possibleFreqs = unkown |> possibleOrientations |> map (\orientation -> (orientation, prod orientation)) |> map (second makeFreqs)
    above12 = possibleFreqs |> map (second (M.filter (>= 12))) |> filter (not . null . snd)
    points :: (Set Point, Point)
    points = case above12 of
      [] -> (S.empty, (0, 0, 0))
      good : _ -> good |> second (head . M.keys) |> (\(ps, trans) -> (S.fromList (map (plus trans) ps), trans))

determineAll :: [[Point]] -> Set Point
determineAll [] = S.empty
determineAll (scanner0 : scanners) = go (S.fromList scanner0) scanners
  where
    go :: Set Point -> [[Point]] -> Set Point
    go known [] = known
    go known (unknown : rest)
      | null newPoints = go known (rest <> [unknown])
      | otherwise = go (S.union known newPoints) rest
      where
        (newPoints, _) = determineOne (toList known) unknown

determineScannerPoses :: [[Point]] -> Set Point
determineScannerPoses [] = S.empty
determineScannerPoses (scanner0 : scanners) = go (S.fromList scanner0, S.singleton (0, 0, 0)) scanners
  where
    go :: (Set Point, Set Point) -> [[Point]] -> Set Point
    go (known, sPoses) [] = sPoses
    go (known, sPoses) (unknown : rest)
      | null newPoints = go (known, sPoses) (rest <> [unknown])
      | otherwise = go (S.union known newPoints, S.insert newScanPos sPoses) rest
      where
        (newPoints, newScanPos) = determineOne (toList known) unknown

manDist :: Point -> Point -> Int
manDist (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

part1 :: String -> Int
part1 inp = scanners |> determineAll |> S.size
  where
    scanners = inp |> pInp |> map snd

part2 :: String -> Int
part2 inp = maximum dists
  where
    scanners = inp |> pInp |> map snd
    scannerPoses = scanners |> determineScannerPoses |> toList
    dists = liftM2 manDist scannerPoses scannerPoses

main :: IO ()
main = do
  inp <- getInput 19
  putAnswer 19 Part1 (part1 inp)
  putAnswer 19 Part2 (part2 inp)