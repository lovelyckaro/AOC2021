{-# LANGUAGE OverloadedStrings #-}
module SantaLib (fetchInput, fetchDescription, getInput, getExample, submitAnswer, putAnswer, readText, module Advent.Types) where

import Advent
import Advent.Types
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as M
import Data.Map (Map, (!?), (!))
import Data.Maybe (fromMaybe)
import Text.HTML.TagSoup

readText :: Read a => Text -> a
readText = read . T.unpack

getOpts :: IO AoCOpts
getOpts = do
  env <- readFile ".env"
  let ls = lines env
  let (year, key) = (read $ head ls, ls !! 1)
  return $ defaultAoCOpts year key

fetchInput :: Integer -> IO ()
fetchInput d = do
  opts <- getOpts
  inp <- runAoC_ opts $ AoCInput (mkDay_ d)
  TIO.writeFile ("input/day" <> show d <> ".input") inp

fetchDescription :: Integer -> IO ()
fetchDescription d = do
  opts <- getOpts
  m <- runAoC_ opts $ AoCPrompt (mkDay_ d)
  TIO.writeFile ("descr/day" <> show d <> "-part1.html") (fromMaybe "Part 1 not unlocked yet" (m !? Part1))
  TIO.writeFile ("descr/day" <> show d <> "-part2.html") (fromMaybe "Part 2 not unlocked yet" (m !? Part2))
  -- hopefully get parse the example. It is usually the first thing within <pre><code> tags.
  let example = pExample (m ! Part1)
  TIO.writeFile ("input/day" <> show d <> "-example.input") example
  return ()

pExample :: Text -> Text
pExample html = fromTagText $ head goal
  where
    tags = parseTags html
    beginsWithPre = partitions (isTagOpenName "pre") tags
    followedByCode = partitions (isTagOpenName "code") <$> beginsWithPre
    inners = head . head $ followedByCode
    goal = filter isTagText inners

getExample :: Int -> IO String
getExample n = readFile ("input/day" <> show n <> "-example.input")

getInput :: Int -> IO String
getInput n = readFile ("input/day" <> show n <> ".input")

submitAnswer :: Integer -> Part -> IO ()
submitAnswer day part = do
  opts <- getOpts
  fp <- case part of
      Part1 -> return ("answer/day" <> show day <> "-part1")
      Part2 -> return ("answer/day" <> show day <> "-part2")
  ans <- readFile fp
  (response, result) <- runAoC_ opts (AoCSubmit (mkDay_ day) part ans)
  TIO.putStrLn response
  print result

putAnswer :: Integer -> Part -> String -> IO ()
putAnswer day part = writeFile fp
  where 
    fp = case part of
      Part1 -> "answer/day" <> show day <> "-part1"
      Part2 -> "answer/day" <> show day <> "-part2"