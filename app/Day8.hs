module Main where
import SantaLib
import Data.Set (Set)
import qualified Data.Set as S
import Data.Bifunctor
import Data.Map (Map, (!?), (!))
import qualified Data.Map as M

data Segment = Top | TopRight | TopLeft | Middle | Bottom | BottomRight | BottomLeft
  deriving (Show, Eq, Ord)

type Key = Map Char Segment

toDigit :: Set Segment -> Int
toDigit s = case m !? s of
  Just n -> n
  Nothing -> error $ "Could not find digit corresponding to " <> show s
  where
    m = M.fromList 
      [ (S.fromList [Top, TopRight, BottomRight, Bottom, BottomLeft, TopLeft], 0)
      , (S.fromList [TopRight, BottomRight], 1) 
      , (S.fromList [Top, TopRight, Middle, BottomLeft, Bottom], 2)
      , (S.fromList [Top, TopRight, Middle, BottomRight, Bottom], 3)
      , (S.fromList [TopLeft, TopRight, Middle, BottomRight], 4)
      , (S.fromList [Top, TopLeft, Middle, BottomRight, Bottom], 5)
      , (S.fromList [Top, TopLeft, Middle, BottomLeft, BottomRight, Bottom], 6)
      , (S.fromList [Top, TopRight, BottomRight], 7)
      , (S.fromList [Top, TopRight, TopLeft, Middle, BottomRight, BottomLeft , Bottom], 8)
      , (S.fromList [Top, TopRight, TopLeft, Middle, BottomRight, Bottom], 9)]

getOne :: [Set Char] -> Set Char
getOne = head . filter (\s -> S.size s == 2)

getSeven :: [Set Char] -> Set Char
getSeven = head . filter (\s -> S.size s == 3)

getEight :: [Set Char] -> Set Char
getEight = head . filter (\s -> S.size s == 7)

getFour :: [Set Char] -> Set Char
getFour = head . filter (\s -> S.size s == 4)

determineTop :: [Set Char] -> (Char, Segment)
determineTop sets = (S.elemAt 0 (seven S.\\ one), Top)
  where 
    one = getOne sets
    seven = getSeven sets

determineBottom :: [Set Char] -> (Char, Segment)
determineBottom sets = (bottom, Bottom)
  where
    four = getFour sets
    seven = getSeven sets
    combined = S.union four seven
    bottom = S.elemAt 0 $ head $ filter (\s -> S.size s == 1) $ map (S.\\ combined) sets

determineMiddle :: [Set Char] -> (Char, Segment)
determineMiddle sets = (middle, Middle)
  where
    threeWithoutMiddle = getOne sets `S.union` S.fromList [fst s | s <- [determineBottom sets, determineTop sets]]
    middle = S.elemAt 0 $ head $ filter (\s -> S.size s == 1) $ map (S.\\ threeWithoutMiddle) sets

getThree :: [Set Char] -> Set Char
getThree sets = getOne sets `S.union` S.fromList [c | (c,_) <- [determineTop sets, determineBottom sets, determineMiddle sets]]

determineBottomLeft :: [Set Char] -> (Char, Segment)
determineBottomLeft sets = (bottomLeft, BottomLeft)
  where
    (topLeft, _) = determineTopLeft sets
    nine = S.insert topLeft $ getThree sets 
    eight = getEight sets
    bottomLeft = S.elemAt 0 $ eight S.\\ nine

determineTopRight :: [Set Char] -> (Char, Segment)
determineTopRight sets = (topRight, TopRight)
  where
    (bottomRight, _) = determineBottomRight sets
    one = getOne sets
    topRight = S.elemAt 0 $ S.delete bottomRight one

determineBottomRight :: [Set Char] -> (Char, Segment)
determineBottomRight sets = (bottomRight, BottomRight)
  where
    (topLeft,_) = determineTopLeft sets
    nine = S.insert topLeft $ getThree sets
    (middle, _) = determineMiddle sets
    six = head $ filter (\s -> S.size s == 6 && s /= nine && S.member middle s) sets
    one = getOne sets
    bottomRight = S.elemAt 0 $ six `S.intersection` one

determineTopLeft :: [Set Char] -> (Char, Segment)
determineTopLeft sets = (topLeft, TopLeft)
  where
    three = getThree sets
    four = getFour sets
    topLeft = S.elemAt 0 $ four S.\\ three

determineAll :: [Set Char] -> Key
determineAll sets = M.fromList 
  [ determineTop sets
  , determineTopRight sets
  , determineTopLeft sets
  , determineMiddle sets
  , determineBottomRight sets
  , determineBottomLeft sets
  , determineBottom sets
  ]

translate :: Key -> [Set Char] -> Int
translate key sets = toInt digits
  where
    segSet = map (S.map (key !)) sets
    digits = map toDigit segSet
    toInt [] = 0
    toInt (x:xs) = x * 10^length xs + toInt xs
  
translateNum :: ([Set Char], [Set Char]) -> Int
translateNum (signals, output) = translate key output
  where key = determineAll signals

pInp :: String -> [([Set Char],[Set Char])]
pInp str = sets
  where
    ls = lines str
    tups = map (break (== '|')) ls
    sets = map (bimap (map S.fromList . words) (map S.fromList . tail . words)) tups

is1478 :: Set Char -> Bool
is1478 s = size == 2 || size == 7 || size == 4 || size == 3
  where size = S.size s

outputs :: [([Set Char], [Set Char])] -> [Set Char]
outputs = concatMap snd

part1 :: String -> Int
part1 = length . filter is1478 . outputs . pInp

part2 :: String -> Int
part2 inp = sum $ map translateNum tups
  where
    tups = pInp inp

main :: IO ()
main = do
  inp <- getInput 8
  putAnswer 8 Part1 (show (part1 inp))
  putAnswer 8 Part2 (show (part2 inp))

