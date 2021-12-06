module Main where
import SantaLib
import Data.List
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

type Amount = Int
type School = IntMap Amount

tickSchool :: School -> School
tickSchool = IM.foldlWithKey age IM.empty
  where
    age im 0 m = IM.insertWith (+) 8 m $ IM.insertWith (+) 6 m im
    age im n m = IM.insertWith (+) (n-1) m im

pInp :: String -> School
pInp s = foldl' (\im age -> IM.insertWith (+) age 1 im) IM.empty ages
  where
    groups = groupBy numeric s
    numeric n1 n2 = n1 `elem` "1234567890" && n2 `elem` "1234567890"
    ages :: [Int]
    ages = map read $ filter (\s -> s /= "," && s /= "\n") groups

part1 :: String -> Int
part1 s = sum (iterate tickSchool initSchool !! 80)
  where
    initSchool = pInp s

part2 :: String -> Int
part2 s = sum (iterate tickSchool initSchool !! 256)
  where
    initSchool = pInp s

main :: IO ()
main = do
  inp <- getInput 6
  putAnswer 6 Part1 (show $ part1 inp)
  putAnswer 6 Part2 (show $ part2 inp)
