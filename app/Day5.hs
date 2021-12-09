module Main where

import Control.Monad (void)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Void (Void)
import SantaLib
  ( Part (Part1, Part2),
    getExample,
    getInput,
    putAnswer,
  )
import Text.Megaparsec
  ( MonadParsec (eof),
    Parsec,
    many,
    parse,
    (<|>),
  )
import Text.Megaparsec.Char (char, eol, string)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

data Line = Line {start :: (Int, Int), end :: (Int, Int)}
  deriving (Show)

pPoint :: Parser (Int, Int)
pPoint = do
  x <- decimal
  char ','
  y <- decimal
  return (x, y)

pLine :: Parser Line
pLine = do
  s <- pPoint
  string " -> "
  e <- pPoint
  void eol <|> eof
  return (Line s e)

pInp :: Parser [Line]
pInp = many pLine

horisontalOrVertical :: Line -> Bool
horisontalOrVertical (Line (x1, y1) (x2, y2)) = x1 == x2 || y1 == y2

diagonal :: Line -> Bool
diagonal (Line (x1, y1) (x2, y2)) = abs (x1 - x2) == abs (y1 - y2)

-- Only works for horisontal or vertical lines, otherwise creates square
pointsInHorisontalOrVertical :: Line -> [(Int, Int)]
pointsInHorisontalOrVertical (Line (x1, y1) (x2, y2)) = do
  x <- [min x1 x2 .. max x1 x2]
  y <- [min y1 y2 .. max y1 y2]
  return (x, y)

pointsInDiagonal :: Line -> [(Int, Int)]
pointsInDiagonal (Line (x1, y1) (x2, y2)) = zip [x1, x1 + xstep .. x2] [y1, y1 + ystep .. y2]
  where
    xstep = signum (x2 - x1)
    ystep = signum (y2 - y1)

pointsInLine :: Line -> [(Int, Int)]
pointsInLine l
  | horisontalOrVertical l = pointsInHorisontalOrVertical l
  | diagonal l = pointsInDiagonal l
  | otherwise = []

pointFreq :: [Line] -> Map (Int, Int) Int
pointFreq = foldr (\p -> M.insertWith (+) p 1) M.empty . concatMap pointsInLine

part1 :: [Line] -> Int
part1 lines = M.size overlaps
  where
    goodLines = filter horisontalOrVertical lines
    freqs = pointFreq goodLines
    overlaps = M.filter (> 1) freqs

part2 :: [Line] -> Int
part2 lines = M.size overlaps
  where
    goodLines = filter (\l -> horisontalOrVertical l || diagonal l) lines
    freqs = pointFreq goodLines
    overlaps = M.filter (> 1) freqs

main :: IO ()
main = do
  inp <- getInput 5
  example <- getExample 5
  let ls = case parse pInp "" inp of
        Left error -> []
        Right lis -> lis
  let ans1 = part1 ls
  let ans2 = part2 ls
  putAnswer 5 Part1 (part1 ls)
  putAnswer 5 Part2 (part2 ls)
