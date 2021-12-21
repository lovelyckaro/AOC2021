{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import SantaLib

pInp :: String -> [Int]
pInp = map (read . T.unpack) . T.splitOn "," . T.pack

cost1 :: Int -> [Int] -> Int
cost1 n xs = sum $ (\x -> abs (x - n)) <$> xs

median :: [Int] -> Int
median xs
  | even len = (ss !! (len `div` 2) + ss !! (len `div` 2)) `div` 2
  | otherwise = ss !! (len `div` 2)
  where
    len = length xs
    ss = sort xs

part1 :: [Int] -> Int
part1 xs = cost1 (median xs) xs

cost2 :: Int -> [Int] -> Int
cost2 n xs = sum $ seriesSum n <$> xs
  where
    seriesSum :: Int -> Int -> Int
    seriesSum n1 n2 =
      let diff = abs (n1 - n2)
       in diff * (diff + 1) `div` 2

mean :: [Int] -> Int
mean xs = sum xs `div` length xs

-- minimum is either ⌊mean xs⌋ or ⌈mean xs⌉
part2 :: [Int] -> Int
part2 xs =
  min
    (cost2 (mean xs) xs)
    (cost2 (mean xs + 1) xs)

main :: IO ()
main = do
  inp <- getInput 7
  let nums = pInp inp
  putAnswer 7 Part1 (part1 nums)
  putAnswer 7 Part2 (part2 nums)
