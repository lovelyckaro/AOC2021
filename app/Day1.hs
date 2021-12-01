module Main where
import SantaLib

part1 :: [Int] -> Int
part1 [] = 0
part1 [x] = 0
part1 (x:y:ys) | x < y = 1 + part1 (y:ys)
               | otherwise = part1 (y:ys)

part2 :: [Int] -> Int
part2 = part1 . windows

windows :: [Int] -> [Int]
windows xs = zipWith3 (\x y z -> x + y + z) xs (tail xs) (tail (tail xs))

test :: [Int]
test = [199,
  200,
  208,
  210,
  200,
  207,
  240,
  269,
  260,
  263]

main :: IO ()
main = do
  inp <- getInput 1
  putAnswer 1 Part1 (show $ part1 (read <$> lines inp))
  putAnswer 1 Part2 (show $ part2 (read <$> lines inp))
