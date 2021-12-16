module Main where
import SantaLib
import Control.Monad.State

data Packet = Literal {version :: Int, value :: Int}
            | Operator {version :: Int, operation :: Op, subpackets :: [Packet] }
  deriving Show

data Op = Sum | Product | Minimum | Maximum | GreaterThan | LessThan | EqualTo
  deriving Show

readN :: Int -> State String String
readN n = state (splitAt n)

pPacket :: State String Packet
pPacket = do
  version <- binary <$> readN 3
  typeId <- binary <$> readN 3
  case typeId of
    4 -> Literal version <$> pLiteral
    _ -> Operator version (typeId |> toOp) <$> pOperator

toOp :: Int -> Op
toOp 0 = Sum
toOp 1 = Product
toOp 2 = Minimum
toOp 3 = Maximum
toOp 5 = GreaterThan
toOp 6 = LessThan
toOp 7 = EqualTo
toOp _ = error "invalid opcode"

pOperator :: State String [Packet]
pOperator = do
  lengthTypeId <- readN 1
  case lengthTypeId of
    "0" -> do
      totalLength <- binary <$> readN 15
      pSubPacketsBin totalLength
    "1" -> do
      numSubPackets <- binary <$> readN 11
      replicateM numSubPackets pPacket
    _ -> undefined

pSubPacketsBin :: Int -> State String [Packet]
pSubPacketsBin bits = do
  lengthBefore <- gets length
  packet <- pPacket
  lengthAfter <- gets length
  let consumedBits = lengthBefore - lengthAfter
  if bits - consumedBits <= 0
    then return [packet]
    else (packet :) <$> pSubPacketsBin (bits - consumedBits)

pLiteral :: State String Int
pLiteral = binary <$> helper ""
  where
    helper soFar = do
      groupHead <- readN 1
      case groupHead of
        "1" -> do
          group <- readN 4
          helper (soFar <> group)
        "0" -> do
          group <- readN 4
          return (soFar <> group)
        _ -> undefined

sumVersionNums :: Packet -> Int
sumVersionNums (Literal v _) = v
sumVersionNums (Operator v _ ps) = v + sum (map sumVersionNums ps)

eval :: Packet -> Int
eval (Literal _ value) = value
eval (Operator _ op ps) = case op of
  Sum -> sum (map eval ps)
  Product -> product (map eval ps)
  Minimum -> minimum (map eval ps)
  Maximum -> maximum (map eval ps)
  GreaterThan -> let [v1, v2] = map eval ps in fromEnum $ v1 > v2
  LessThan -> let [v1,v2] = map eval ps in fromEnum $ v1 < v2
  EqualTo -> let [v1,v2] = map eval ps in fromEnum $ v1 == v2

binary :: String -> Int
binary [] = 0
binary ('0':xs) = binary xs
binary ('1':xs) = 2^length xs + binary xs
binary (_:xs) = undefined

toBin :: Char -> String
toBin '0' = "0000"
toBin '1' = "0001"
toBin '2' = "0010"
toBin '3' = "0011"
toBin '4' = "0100"
toBin '5' = "0101"
toBin '6' = "0110"
toBin '7' = "0111"
toBin '8' = "1000"
toBin '9' = "1001"
toBin 'A' = "1010"
toBin 'B' = "1011"
toBin 'C' = "1100"
toBin 'D' = "1101"
toBin 'E' = "1110"
toBin 'F' = "1111"
toBin _ = ""

part1 :: String -> Int
part1 inp = inp >>= toBin |> evalState pPacket |> sumVersionNums

part2 :: String -> Int
part2 inp = inp >>= toBin |> evalState pPacket |> eval

main :: IO ()
main = do
  inp <- getInput 16
  putAnswer 16 Part1 (part1 inp)
  putAnswer 16 Part2 (part2 inp) 
