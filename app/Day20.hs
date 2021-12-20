{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Main where
import SantaLib
import Data.Set (Set)
import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Foldable
import Data.Ord
data Pixel = Dark | Light
  deriving (Show, Eq)
type Point = (Int, Int)
type ImageEnhancer = Vector Pixel
data Bounds = Bounds {minrow, maxrow, mincol, maxcol :: Int}
  deriving Show

outsideBounds :: Bounds -> Point -> Bool
outsideBounds Bounds {..} (row, col) = row < minrow || row > maxrow || col < mincol || col > maxcol
data Image = Image {image :: Set Point, bounds :: Bounds, defaultOutside :: Pixel }
  deriving Show

opposite :: Pixel -> Pixel
opposite = \case
  Dark -> Light
  Light -> Dark

pPixel :: Char -> Pixel
pPixel '#' = Light
pPixel '.' = Dark
pPixel _ = undefined

pInp :: String -> (ImageEnhancer, Image)
pInp string = (imageEnhancer,Image s bounds Dark)
  where
    ls = lines string
    imageEnhancer = ls |> head |> map pPixel |> V.fromList
    matrix = ls |> drop 2 |> map V.fromList |> V.fromList
    s = matrix |> V.ifoldl' (\set rowNum rowVect -> set `S.union` V.ifoldl' (\innerSet colNum value -> if pPixel value == Light then S.insert (rowNum, colNum) innerSet else innerSet) S.empty rowVect) S.empty
    minrow = fst (minimumBy (comparing fst) s)
    maxrow = fst (maximumBy (comparing fst) s)
    mincol = snd (minimumBy (comparing snd) s)
    maxcol = snd (maximumBy (comparing snd) s)
    bounds = Bounds {..}

toInt :: [Pixel] -> Int
toInt [] = 0
toInt (Dark:rest) = toInt rest
toInt (Light:rest) = 2^length rest + toInt rest

readNeighbors :: Image -> Point -> Int
readNeighbors (Image s bounds outside) (row,col) = toInt [lookup (row',col') s | row' <- [row - 1, row, row + 1], col' <- [col - 1, col, col + 1]]
  where
    lookup p s | outsideBounds bounds p = outside
               | S.member p s = Light
               | otherwise    = Dark

tick :: ImageEnhancer -> Image -> Image
tick imageEnhancer (Image image Bounds {..} outside) = Image (foldr handlePoint S.empty ps) (Bounds (minrow - 1) (maxrow + 1) (mincol - 1) (maxcol + 1)) (opposite outside)
  where
    ps = [(row,col) | row <- [minrow -1 .. maxrow + 1], col <- [mincol -1 .. maxcol + 1]]
    handlePoint :: Point -> Set Point -> Set Point
    handlePoint point acc = case imageEnhancer V.! readNeighbors (Image image Bounds {..} outside) point of
      Dark -> acc
      Light -> S.insert point acc

part1 :: String -> Int
part1 inp = i |> tick' |> tick' |> image |> S.size
  where
    (enhancer, i) = pInp inp
    tick' = tick enhancer

part2 :: String -> Int
part2 inp = i |> iterate tick' |> (!! 50) |> image |> S.size
  where
    (enhancer, i) = pInp inp
    tick' = tick enhancer

main :: IO ()
main = do
  inp <- getInput 20
  putAnswer 20 Part1 (part1 inp)
  putAnswer 20 Part2 (part2 inp)