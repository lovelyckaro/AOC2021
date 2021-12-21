{-# LANGUAGE BinaryLiterals #-}

module Main where

import Data.Bits
import SantaLib

readLine :: String -> Int
readLine [] = 0
readLine ('0' : xs) = readLine xs
readLine ('1' : xs) = 2 ^ length xs + readLine xs
readLine _ = error "unreachable"

readInp :: String -> [Int]
readInp = map readLine . lines

mostCommon :: Bits a => Int -> [a] -> a
mostCommon n xs =
  let bits = map (`testBit` n) xs
   in if length (filter id bits) >= length (filter not bits)
        then bit n
        else zeroBits

leastCommon :: Bits a => Int -> [a] -> a
leastCommon n xs = complementBit (mostCommon n xs) n

gamma :: [Int] -> Int
gamma xs = sum [mostCommon bit xs | bit <- [0 .. 11]]

epsilon :: [Int] -> Int
epsilon xs = sum [leastCommon bit xs | bit <- [0 .. 11]]

part1 :: String -> Int
part1 inp = gamma bs * epsilon bs
  where
    bs = readInp inp

oxygen :: [Int] -> Int
oxygen = head . go 11
  where
    go _ [] = error "Failure"
    go _ [x] = [x]
    go bit xs = go (bit - 1) (filter (\word -> testBit word bit == testBit wanted bit) xs)
      where
        wanted = mostCommon bit xs

co2 :: [Int] -> Int
co2 = head . go 11
  where
    go _ [] = error "Failure"
    go _ [x] = [x]
    go bit xs = go (bit - 1) (filter (\word -> testBit word bit == testBit wanted bit) xs)
      where
        wanted = leastCommon bit xs

part2 :: String -> Int
part2 inp = co2 xs * oxygen xs
  where
    xs = readInp inp

main :: IO ()
main = do
  inp <- getInput 3
  putAnswer 3 Part1 (part1 inp)
  putAnswer 3 Part2 (part2 inp)

test :: [Int]
test =
  [ 0b00100,
    0b11110,
    0b10110,
    0b10111,
    0b10101,
    0b01111,
    0b00111,
    0b11100,
    0b10000,
    0b11001,
    0b00010,
    0b01010
  ]
