{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (void)
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Data.Void
import SantaLib
import Test.QuickCheck
import Text.Megaparsec hiding (getInput)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)
import Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void String

type Point = (Int, Int, Int)

data Region = Region {lowx, highx, lowy, highy, lowz, highz :: Int}
  deriving (Show)

instance Arbitrary Region where
  arbitrary = Region <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

type Input = (Instruction, Region)

data Instruction = On | Off
  deriving (Show, Eq, Ord)

volume :: Region -> Int
volume Region {..} = product [highx - lowx + 1, highy - lowy + 1, highz - lowz + 1]

overlap :: Region -> Region -> Maybe Region
overlap (Region lowx1 highx1 lowy1 highy1 lowz1 highz1) (Region lowx2 highx2 lowy2 highy2 lowz2 highz2) =
  do
    (lowx, highx) <- xOverlap
    (lowy, highy) <- yOverlap
    (lowz, highz) <- zOverlap
    return (Region {..})
  where
    xOverlap = sideOverlap lowx1 highx1 lowx2 highx2
    yOverlap = sideOverlap lowy1 highy1 lowy2 highy2
    zOverlap = sideOverlap lowz1 highz1 lowz2 highz2
    sideOverlap low1 high1 low2 high2 =
      let (low', high') = (max low1 low2, min high1 high2)
       in if low' <= high' then Just (low', high') else Nothing

{- ORMOLU_DISABLE -}
difference :: Region -> Region -> [Region]
difference r1 r2 = case overlap r1 r2 of
  Nothing -> [r1]
  Just roverlap -> filter (\Region{..} -> lowx <= highx && lowy <= highy && lowz <= highz) r'
    where

      r' =
        [ Region (lowx r1)            (highx r1)          (lowy r1)            (highy r1)          (lowz r1)            (lowz roverlap - 1)
        , Region (lowx r1)            (highx r1)          (lowy r1)            (highy r1)          (highz roverlap + 1) (highz r1)
        , Region (lowx r1)            (lowx roverlap - 1) (lowy r1)            (highy r1)          (lowz roverlap)      (highz roverlap)
        , Region (highx roverlap + 1) (highx r1)          (lowy r1)            (highy r1)          (lowz roverlap)      (highz roverlap)
        , Region (lowx roverlap)      (highx roverlap)    (lowy r1)            (lowy roverlap - 1) (lowz roverlap)      (highz roverlap)
        , Region (lowx roverlap)      (highx roverlap)    (highy roverlap + 1) (highy r1)          (lowz roverlap)      (highz roverlap)
        ]
{- ORMOLU_ENABLE -}

propVolumeDifference :: Region -> Region -> Bool
propVolumeDifference r1 r2 = case overlap r1 r2 of
  Nothing -> True
  Just roverlap -> sum (map volume (difference r1 r2)) == volume r1 - volume roverlap

withinFifty :: [Input] -> [Input]
withinFifty = map cap
  where
    cap (s, Region {..}) = (s, Region (gt lowx) (lt highx) (gt lowy) (lt highy) (gt lowz) (lt highz))
    gt = max (-50)
    lt = min 50

startup :: [Input] -> [Region]
startup = foldl' handleInput []
  where
    handleInput [] (Off, _) = []
    handleInput [] (On, region) = [region]
    handleInput blocks (Off, region) = concatMap (`difference` region) blocks
    handleInput blocks (On, region) = blocks <> remaining
      where
        remaining :: [Region]
        remaining = foldl removeOverlap [region] blocks
        removeOverlap :: [Region] -> Region -> [Region]
        removeOverlap regions remove = concatMap (`difference` remove) regions

pInp :: Parser [Input]
pInp = many pLine
  where
    pInstr = try (string "on" >> return On) <|> (string "off" >> return Off)
    pLine = do
      instr <- pInstr
      string " x="
      lowx <- pSigned
      string ".."
      highx <- pSigned
      string ",y="
      lowy <- pSigned
      string ".."
      highy <- pSigned
      string ",z="
      lowz <- pSigned
      string ".."
      highz <- pSigned
      void eol <|> eof
      return (instr, Region {..})
    pSigned = signed (void space) decimal

part1 :: [Input] -> Int
part1 regions = regions |> withinFifty |> startup |> map volume |> sum

part2 :: [Input] -> Int
part2 regions = regions |> startup |> map volume |> sum

main :: IO ()
main = do
  inp <- getInput 22
  regions <- case parse pInp "input" inp of
    Left error -> fail (errorBundlePretty error)
    Right regions -> return regions
  putAnswer 22 Part1 (part1 regions)
  putAnswer 22 Part2 (part2 regions)