{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (void)
import Data.Foldable (Foldable (foldl'))
import Data.Void (Void)
import SantaLib (Part (Part1, Part2), getInput, putAnswer)
import Text.Megaparsec
  ( MonadParsec (eof),
    Parsec,
    errorBundlePretty,
    many,
    parse,
    parseTest,
    (<|>),
  )
import Text.Megaparsec.Char (newline, space, string)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

data Instr = Down Integer | Up Integer | Forward Integer
  deriving (Show)

data Pos1 = Pos1 {depth :: Integer, forwardDistance :: Integer}
  deriving (Show)

instance Semigroup Pos1 where
  Pos1 d1 f1 <> Pos1 d2 f2 = Pos1 (d1 + d2) (f1 + f2)

instance Monoid Pos1 where
  mempty = Pos1 0 0

pInstr :: Parser Instr
pInstr = do
  dir <- string "forward" <|> string "up" <|> string "down"
  space
  n <- decimal
  eof <|> void newline
  case dir of
    "forward" -> return $ Forward n
    "up" -> return $ Up n
    "down" -> return $ Down n
    _ -> error "unreachable"

pInp :: Parser [Instr]
pInp = many pInstr

toPos :: [Instr] -> Pos1
toPos = foldMap $ \case
  Up n -> Pos1 (- n) 0
  Down n -> Pos1 n 0
  Forward n -> Pos1 0 n

part1 :: [Instr] -> Integer
part1 inp = depth pos * forwardDistance pos
  where
    pos = toPos inp

data Pos2 = Pos2 {aim :: Integer, d :: Integer, forward :: Integer}
  deriving (Show)

toPos2 :: [Instr] -> Pos2
toPos2 = foldl' fun (Pos2 0 0 0)
  where
    fun (Pos2 aim depth forward) = \case
      Up n -> Pos2 (aim - n) depth forward
      Down n -> Pos2 (aim + n) depth forward
      Forward n -> Pos2 aim (depth + aim * n) (forward + n)

part2 :: [Instr] -> Integer
part2 inp = d pos * forward pos
  where
    pos = toPos2 inp

main :: IO ()
main = do
  inp <- getInput 2
  instrs <- case parse pInp "input" inp of
    Left error -> putStrLn (errorBundlePretty error) >> return []
    Right value -> return value
  putAnswer 2 Part1 (part1 instrs)
  putAnswer 2 Part2 (part2 instrs)