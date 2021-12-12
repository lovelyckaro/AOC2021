module Main where
import SantaLib
import Data.Map (Map, (!?))
import qualified Data.Map as M
import Data.List.Split
import Data.Char
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad

data Node = Lower String | Upper String | Start | End
  deriving (Ord, Eq, Show)

type Graph = Map Node [Node]
type Path = [Node]

pInp :: String -> Graph
pInp inp = foldr (\(start, end) -> M.insertWith (<>) end [start] . M.insertWith (<>) start [end]) M.empty ls
  where
    ls = lines inp |> map pLine

pLine :: [Char] -> (Node, Node)
pLine l = case splitOn "-" l of
  [start, end] -> (pNode start, pNode end)
  _ -> undefined

pNode :: [Char] -> Node
pNode "start" = Start
pNode "end" = End
pNode xs | head xs |> isLower = Lower xs
         | otherwise = Upper xs

findAllPaths :: Graph -> [Path]
findAllPaths g = dfs [Start] (S.singleton Start)
  where
    dfs [] _ = undefined
    dfs (latest : curPath) visiting = do
      outNode <- M.findWithDefault [] latest g
      guard (not $ S.member outNode visiting) 
      case outNode of
        Lower s -> dfs (outNode : latest : curPath) (S.insert outNode visiting)
        Upper s -> dfs (outNode : latest : curPath) visiting
        Start -> undefined
        End -> return (outNode : latest : curPath)

findAllPaths2 :: Graph -> [Path]
findAllPaths2 g = dfs [Start] (S.singleton Start) True
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
part1 inp = inp |> pInp |> findAllPaths |> length

part2 :: String -> Int
part2 inp = inp |> pInp |> findAllPaths2 |> length

main :: IO ()
main = do
  inp <- getInput 12
  putAnswer 12 Part1 (part1 inp)
  putAnswer 12 Part2 (part2 inp)
