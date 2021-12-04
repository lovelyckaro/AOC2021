{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
module Main where
import SantaLib ( getInput, putAnswer, Part(Part2, Part1) )
import Text.Megaparsec ( parse, errorBundlePretty, many, Parsec )
import Text.Megaparsec.Char ( char, eol, space )
import Text.Megaparsec.Char.Lexer (decimal)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Set (Set)
import Data.Map (Map, (!?))
import Data.Void ( Void )
import Text.Megaparsec.Error (errorBundlePretty)

type Pos = (Int, Int)
data Board = Board { locations :: Map Int Pos, marked :: Map Pos Int,  latestDrawn :: Int}
  deriving Show

winningSets :: [Set Pos]
winningSets = do
  row <- [0..4]
  let rowSets = S.fromList ((row,) <$> [0..4])
  let colSets = S.fromList ((,row) <$> [0..4])
  [rowSets, colSets]
  

hasWon :: Board -> Bool
hasWon Board{..} = any (`S.isSubsetOf` m) winningSets
  where
    m = M.keysSet marked

mark :: Int -> Board -> Board
mark n board = board {latestDrawn = n, marked = marked'}
  where 
    marked' = case locations board !? n of
      Just pos -> M.insert pos n (marked board)
      Nothing -> marked board 

markUntilWinner :: [Int] -> [Board] -> Board
markUntilWinner [] _ = error "ran out of numbers"
markUntilWinner (x:xs) boards 
  | any hasWon boards' = head (filter hasWon boards')
  | otherwise = markUntilWinner xs boards'
  where
    boards' = mark x <$> boards

markUntilLastWinner :: [Int] -> [Board] -> Board
markUntilLastWinner [] _ = error "ran out of numbers"
markUntilLastWinner (x:xs) [board] 
  | hasWon board = board
  | otherwise = markUntilLastWinner xs [mark x board]
markUntilLastWinner (x:xs) boards = markUntilLastWinner xs boards'
  where
    marked = mark x <$> boards
    boards' = filter (not . hasWon) marked


unmarked :: Board -> Set Int
unmarked (Board locs ms _) = M.keysSet locs S.\\ S.fromList (M.elems ms)

part1 :: [Int] -> [Board] -> Int
part1 ns bs = sum (unmarked winner) * latestDrawn winner
  where
    winner = markUntilWinner ns bs

part2 :: [Int] -> [Board] -> Int
part2 ns bs = sum (unmarked last) * latestDrawn last
  where
    last = markUntilLastWinner ns bs


type Parser = Parsec Void String

pNums :: Parser [Int]
pNums = do
  n1 <- decimal
  rest <- many (char ',' >> decimal)
  eol
  return (n1 : rest)


pBoard :: Parser Board
pBoard = do
  rows <- mapM pRow [0..4]
  let ls = foldr M.union M.empty rows
  return (Board ls M.empty (-1))

pRow :: Int -> Parser (Map Int Pos)
pRow row = do
  ns <- mapM (\_ -> space >> decimal) [0..4]
  eol
  let poses = (row,) <$> [0..4]
  return $ M.fromList $ zip ns poses

pBoards :: Parser [Board]
pBoards = many (eol >> pBoard)

pInp :: Parser ([Int], [Board])
pInp = (,) <$> pNums <*> pBoards


main :: IO ()
main = do
  inp <- getInput 4
  case parse pInp "input" inp of
    Left error -> putStrLn (errorBundlePretty error)
    Right (nums, boards) -> do
      putAnswer 4 Part1 (show $ part1 nums boards)
      putAnswer 4 Part2 (show $ part2 nums boards)

test = unlines [
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1",
  "",
  "22 13 17 11  0",
  " 8  2 23  4 24",
  "21  9 14 16  7",
  " 6 10  3 18  5",
  " 1 12 20 15 19",
  "",
  " 3 15  0  2 22",
  " 9 18 13 17  5",
  "19  8  7 25 23",
  "20 11 10 24  4",
  "14 21 16 12  6",
  "",
  "14 21 17 24  4",
  "10 16 15  9 19",
  "18  8 23 26 20",
  "22 11 13  6  5",
  " 2  0 12  3  7"
  ]