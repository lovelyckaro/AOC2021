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
    toFoldInstr str = case stripPrefix "fold along " str of
      Just ('x' : '=' : num) -> AlongX (read num)
      Just ('y' : '=' : num) -> AlongY (read num)
      _ -> undefined
    toPoint [row, col] = (read row, read col)
    toPoint _ = undefined

foldAlong :: FoldInstr -> Set Point -> Set Point
foldAlong (AlongX n) = S.map (\(x, y) -> if x > n then (n - (x - n), y) else (x, y))
foldAlong (AlongY n) = S.map (\(x, y) -> if y > n then (x, n - (y - n)) else (x, y))

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
    ls = map (\y -> map (\x -> if S.member (x, y) s then '█' else '░') [minX .. maxX]) [minY .. maxY]
    minX = S.findMin (S.map fst s)
    minY = S.findMin (S.map snd s)
    maxX = S.findMax (S.map fst s)
    maxY = S.findMax (S.map snd s)

main :: IO ()
main = do
  inp <- getInput 13
  putAnswer 13 Part1 (part1 inp)
  writeFile "answer/day13-part2" (part2 inp)