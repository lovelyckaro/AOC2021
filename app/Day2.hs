{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import SantaLib
import Data.List
import Data.Text (Text, breakOn, pack)
import qualified Data.Text as T

data Instr = Down Integer | Up Integer | Forward Integer
  deriving Show

data Pos1 = Pos1 {depth :: Integer, forwardDistance :: Integer}
  deriving Show

instance Semigroup Pos1 where
  Pos1 d1 f1 <> Pos1 d2 f2 = Pos1 (d1 + d2) (f1 + f2)

instance Monoid Pos1 where
  mempty = Pos1 0 0

pInstr :: Text -> Instr
pInstr s = dir len
  where
    (dirStr, lenStr) = breakOn " " s 
    dir = pDir dirStr
    len = readText lenStr

pDir :: Text -> (Integer -> Instr)
pDir s = case s of
  "forward" -> Forward
  "up" -> Up
  "down" -> Down
  _ -> error "did not match forward, up or down"


pInp :: Text -> [Instr]
pInp = map pInstr . T.lines

toPos :: [Instr] -> Pos1
toPos = foldMap $ \case
  Up n -> Pos1 (-n) 0
  Down n -> Pos1 n 0
  Forward n -> Pos1 0 n

part1 :: String -> Integer
part1 inp = depth pos * forwardDistance pos
  where 
    pos = toPos . pInp . pack $ inp

data Pos2 = Pos2 {aim :: Integer, d :: Integer, forward :: Integer}
  deriving Show

toPos2 :: [Instr] -> Pos2
toPos2 = foldl' fun (Pos2 0 0 0)
  where 
    fun (Pos2 aim depth forward) = \case
      Up n -> Pos2 (aim - n) depth forward
      Down n -> Pos2 (aim + n) depth forward
      Forward n -> Pos2 aim (depth + aim * n) (forward + n)

part2 :: String -> Integer
part2 inp = d pos * forward pos
  where
    pos = toPos2 . pInp . pack $ inp

main :: IO ()
main = do
  inp <- getInput 2
  putAnswer 2 Part1 (show $ part1 inp) 
  putAnswer 2 Part2 (show $ part2 inp)