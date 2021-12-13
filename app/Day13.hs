module Main where

import Data.List (foldl', stripPrefix)
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as S
import SantaLib

data FoldInstr = AlongX Int | AlongY Int
  deriving (Show)

type Point = (Int, Int)

pInp :: String -> (Set Point, [FoldInstr])
pInp inp = (points |> lines |> map (splitOn ",") |> map toPoint |> S.fromList, folds |> lines |> map toFoldInstr)
  where
    [points, folds] = splitOn "\n\n" inp
    toFoldInstr str = case (stripPrefix "fold along x=" str, stripPrefix "fold along y=" str) of
      (Nothing, Nothing) -> error $ "Could not parse" <> str
      (Just x, Nothing) -> AlongX (read x)
      (Nothing, Just y) -> AlongY (read y)
      (Just _, Just _) -> error "unreachable"
    toPoint [row, col] = (read row, read col)
    toPoint _ = undefined

foldAlong :: FoldInstr -> Set Point -> Set Point
foldAlong (AlongX n) s = (s S.\\ toBeFolded) `S.union` folded
  where
    toBeFolded = S.filter (\(x, y) -> x > n) s
    folded :: Set Point
    folded = S.map (\(x, y) -> (n - (x - n), y)) toBeFolded
foldAlong (AlongY n) s = (s S.\\ toBeFolded) `S.union` folded
  where
    toBeFolded = S.filter (\(x, y) -> y > n) s
    folded = S.map (\(x, y) -> (x, n - (y - n))) toBeFolded

part1 :: String -> Int
part1 inp = points |> foldAlong (head foldInstrs) |> S.size
  where
    (points, foldInstrs) = pInp inp

part2 :: String -> String
part2 inp = foldl' (flip foldAlong) points foldInstrs |> prettify
  where
    (points, foldInstrs) = pInp inp

prettify :: Set Point -> String
prettify s = unlines ls
  where
    ls = map (\y -> map (\x -> if S.member (x, y) s then '#' else '.') [minX .. maxX]) [minY .. maxY]
    minX = S.findMin (S.map fst s)
    minY = S.findMin (S.map snd s)
    maxX = S.findMax (S.map fst s)
    maxY = S.findMax (S.map snd s)

main :: IO ()
main = do
  inp <- getInput 13
  putAnswer 13 Part1 (part1 inp)
  writeFile "answer/day13-part2" (part2 inp)