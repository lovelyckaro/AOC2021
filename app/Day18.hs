module Main where

import Control.Monad
import Data.Void
import SantaLib
import Text.Megaparsec hiding (getInput)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

type Parser = Parsec Void String

data SnailFish = Normal Int | Pair SnailFish SnailFish

instance Show SnailFish where
  show (Normal n) = show n
  show (Pair l r) = "[" <> show l <> "," <> show r <> "]"

pInp :: Parser [SnailFish]
pInp = many (pSnailFish >>= \fish -> eol >> return fish)

pSnailFish :: Parser SnailFish
pSnailFish = try pNormal <|> pPair

pNormal :: Parser SnailFish
pNormal = Normal <$> decimal

pPair :: Parser SnailFish
pPair = do
  char '['
  l <- pSnailFish
  char ','
  r <- pSnailFish
  char ']'
  return $ Pair l r

addRightMost :: Int -> SnailFish -> SnailFish
addRightMost n (Normal n1) = Normal (n + n1)
addRightMost n (Pair l r) = Pair l (addRightMost n r)

addLeftMost :: Int -> SnailFish -> SnailFish
addLeftMost n (Normal n1) = Normal (n + n1)
addLeftMost n (Pair l r) = Pair (addLeftMost n l) r

data ExplodeResult = None | Done SnailFish | AddBoth Int Int SnailFish | AddRight Int SnailFish | AddLeft Int SnailFish
  deriving (Show)

explode :: SnailFish -> ExplodeResult
explode = go 0
  where
    go :: Int -> SnailFish -> ExplodeResult
    go 4 (Pair (Normal l) (Normal r)) = AddBoth l r (Normal 0)
    go 4 (Pair l r) = error "trying to explode pair which consists of pairs"
    go depth (Pair l r) = case go (depth + 1) l of
      (Done l') -> Done (Pair l' r)
      (AddBoth nl nr l') -> AddLeft nl (Pair l' (addLeftMost nr r))
      (AddRight nr l') -> Done (Pair l' (addLeftMost nr r))
      (AddLeft nl l') -> AddLeft nl (Pair l' r)
      None -> case go (depth + 1) r of
        (Done r') -> Done (Pair l r')
        (AddBoth nl nr r') -> AddRight nr (Pair (addRightMost nl l) r')
        (AddRight nr r') -> AddRight nr (Pair l r')
        (AddLeft nl r') -> Done (Pair (addRightMost nl l) r')
        None -> None
    go _ (Normal n) = None

data SplitResult = NoSplit | DoneSplit SnailFish
  deriving (Show)

split :: SnailFish -> SplitResult
split (Normal n)
  | n >= 10 = DoneSplit $ Pair (Normal $ n `div` 2) (Normal $ (n + 1) `div` 2)
  | otherwise = NoSplit
split (Pair l r) = case split l of
  DoneSplit l' -> DoneSplit (Pair l' r)
  NoSplit -> case split r of
    DoneSplit r' -> DoneSplit (Pair l r')
    NoSplit -> NoSplit

reduce :: SnailFish -> SnailFish
reduce sn = case explode sn of
  Done sn' -> reduce sn'
  AddBoth {} -> error "The impossible happened"
  AddRight _ sn' -> reduce sn'
  AddLeft _ sn' -> reduce sn'
  None -> case split sn of
    DoneSplit sn' -> reduce sn'
    NoSplit -> sn

add :: SnailFish -> SnailFish -> SnailFish
add l r = reduce (Pair l r)

magnitude :: SnailFish -> Int
magnitude (Normal n) = n
magnitude (Pair l r) = magnitude l * 3 + magnitude r * 2

part1 :: [SnailFish] -> Int
part1 fishes = foldl1 add fishes |> magnitude

part2 :: [SnailFish] -> Int
part2 fishes = maximum $ magnitude <$> (add <$> fishes <*> fishes)

main :: IO ()
main = do
  inp <- getInput 18
  fishes <- case parse pInp "input" inp of
    Left error -> fail $ errorBundlePretty error
    Right sfs -> return sfs
  putAnswer 18 Part1 (part1 fishes)
  putAnswer 18 Part2 (part2 fishes)
