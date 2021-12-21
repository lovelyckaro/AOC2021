{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.Except
import Control.Monad.State
import Data.Foldable
import Data.List
import SantaLib

data BlockType = Paren | Square | Curly | Spike
  deriving (Show, Eq)

data BlockError = WrongTerminator BlockType | ClosingEmpty
  deriving (Show)

type ParseStack = [BlockType]

type ParenM = StateT ParseStack (Except BlockError)

initState :: ParseStack
initState = []

evalParenM :: ParenM a -> Either BlockError a
evalParenM p = runExcept $ evalStateT p initState

runParenM :: ParenM a -> Either BlockError (a, ParseStack)
runParenM p = runExcept $ runStateT p initState

push :: BlockType -> ParenM ()
push bt = do
  stack <- get
  put (bt : stack)

pop :: BlockType -> ParenM ()
pop bt = do
  stack <- get
  case stack of
    [] -> throwError ClosingEmpty
    (x : xs)
      | x == bt -> put xs
      | otherwise -> throwError $ WrongTerminator bt

handleChar :: Char -> ParenM ()
handleChar = \case
  '(' -> push Paren
  ')' -> pop Paren
  '[' -> push Square
  ']' -> pop Square
  '{' -> push Curly
  '}' -> pop Curly
  '<' -> push Spike
  '>' -> pop Spike
  _ -> undefined

handleLine :: [Char] -> Either BlockError ((), ParseStack)
handleLine = runParenM . mapM_ handleChar

corruptScore :: Either BlockError ((), ParseStack) -> Int
corruptScore = \case
  Left (WrongTerminator bt) -> case bt of
    Paren -> 3
    Square -> 57
    Curly -> 1197
    Spike -> 25137
  Left ClosingEmpty -> undefined
  Right ((), _) -> 0

incompleteScore :: Either BlockError ((), ParseStack) -> Int
incompleteScore = \case
  Left _ -> 0
  Right ((), xs) -> foldl' (\n pb -> 5 * n + typeToScore pb) 0 xs
    where
      typeToScore = \case
        Paren -> 1
        Square -> 2
        Curly -> 3
        Spike -> 4

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

part1 :: String -> Int
part1 inp = inp |> lines |> map handleLine |> map corruptScore |> sum

part2 inp = inp |> lines |> map handleLine |> map incompleteScore |> filter (/= 0) |> sort |> middle

main :: IO ()
main = do
  inp <- getInput 10
  ex <- getExample 10
  print (part2 ex)
  putAnswer 10 Part1 (part1 inp)
  putAnswer 10 Part2 (part2 inp)
