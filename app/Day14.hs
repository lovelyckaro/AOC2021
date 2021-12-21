module Main where

import Data.List
import Data.List.Split
import Data.Map (Map, (!?))
import qualified Data.Map as M
import SantaLib

type Polymer = Map String Integer

pInp :: String -> (Polymer, Map String Char, Char)
pInp inp = (polymer, mappings |> lines |> map toTup |> M.fromList, last startPoint)
  where
    [startPoint, mappings] = splitOn "\n\n" inp
    polymer = M.fromListWith (+) $ zip (zipWith (\a b -> [a, b]) startPoint (tail startPoint)) (repeat 1)
    toTup line = let [have, get] = splitOn " -> " line in (have, head get)

tick :: Map String Char -> Polymer -> Polymer
tick mapping = M.foldrWithKey go M.empty
  where
    go :: String -> Integer -> Polymer -> Polymer
    go pair currentAmount acc = case mapping !? pair of
      Nothing -> M.insert pair currentAmount acc
      Just c -> acc |> M.insertWith (+) [head pair, c] currentAmount |> M.insertWith (+) [c, last pair] currentAmount

toCharCount :: Polymer -> Map Char Integer
toCharCount = M.foldrWithKey go M.empty
  where
    go :: String -> Integer -> Map Char Integer -> Map Char Integer
    go [a, b] amount m = m |> M.insertWith (+) a amount --  |> M.insertWith (+) b amount
    go _ _ _ = undefined

part1 :: String -> Integer
part1 inp = maximum charCount - minimum charCount
  where
    (polymer, mapping, lastChar) = pInp inp
    tenthIter = iterate (tick mapping) polymer !! 10
    charCount = M.insertWith (+) lastChar 1 $ toCharCount tenthIter

part2 :: String -> Integer
part2 inp = maximum charCount - minimum charCount
  where
    (polymer, mapping, lastChar) = pInp inp
    fortythIter = iterate (tick mapping) polymer !! 40
    charCount = M.insertWith (+) lastChar 1 $ toCharCount fortythIter

main :: IO ()
main = do
  inp <- getInput 14
  putAnswer 14 Part1 (part1 inp)
  putAnswer 14 Part2 (part2 inp)
