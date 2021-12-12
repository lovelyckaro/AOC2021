{-# LANGUAGE LambdaCase #-}
module Main where
import SantaLib
import Data.Map (Map)
import qualified Data.Map as M
import Data.List.Split ( splitOn )
import Data.Char ( isLower )
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad ( guard )

data Node = Lower String | Upper String | Start | End
  deriving (Ord, Eq, Show)

type Graph = Map Node [Node]
type Path = [Node]

pInp :: String -> Graph
pInp inp = foldr (\(start, end) -> M.insertWith (<>) end [start] . M.insertWith (<>) start [end]) M.empty ls
  where
    ls = lines inp |> map pLine
    pLine l = let [start, end] = splitOn "-" l in (pNode start, pNode end)
    pNode = \case
      "start" -> Start
      "end" -> End
      xs | head xs |> isLower -> Lower xs
         | otherwise -> Upper xs

findAllPaths :: Bool -> Graph -> [Path]
findAllPaths canGoAgain g = dfs [Start] (S.singleton Start) canGoAgain
  where
    dfs [] _ _ = undefined
    dfs (latest : curPath) visiting canGoAgain = do
      outNode <- M.findWithDefault [] latest g 
      guard (not (S.member outNode visiting) || (canGoAgain && outNode /= Start))
      let canGoAgain' = canGoAgain && not (S.member outNode visiting)
      case outNode of
        Lower s -> dfs (outNode : latest : curPath) (S.insert outNode visiting) canGoAgain'
        Upper s -> dfs (outNode : latest : curPath) visiting canGoAgain'
        Start -> undefined
        End -> return (outNode : latest : curPath)

part1 :: String -> Int
part1 inp = inp |> pInp |> findAllPaths False |> length

part2 :: String -> Int
part2 inp = inp |> pInp |> findAllPaths True |> length

main :: IO ()
main = do
  inp <- getInput 12
  putAnswer 12 Part1 (part1 inp)
  putAnswer 12 Part2 (part2 inp)
