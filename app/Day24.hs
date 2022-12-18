{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative (asum)
import Control.Monad
import Control.Monad.State
import Data.List (isPrefixOf)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.SBV
import Data.SBV.Tools.Overflow
import Data.Void
import SantaLib
import System.Exit (exitFailure)
import Text.Megaparsec hiding (getInput)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

symbol :: String -> Parser String
symbol = L.symbol hspace

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

lexemeLn :: Parser a -> Parser a
lexemeLn = L.lexeme (void eol)

type Register = Char

type Value = SInt64

data Operand = Reg Register | Val Value
  deriving (Show)

data Instruction
  = Inp Register
  | Add Register Operand
  | Mul Register Operand
  | Div Register Operand
  | Mod Register Operand
  | Eql Register Operand
  deriving (Show)

pRegister :: Parser Register
pRegister = lexeme (oneOf "wxyz")

pOperand :: Parser Operand
pOperand = Reg <$> pRegister <|> Val <$> lexeme (L.signed hspace L.decimal)

pInstruction :: Parser Instruction
pInstruction =
  asum . map try $
    [ Inp <$> (symbol "inp" >> pRegister),
      Add <$> (symbol "add" >> pRegister) <*> pOperand,
      Mul <$> (symbol "mul" >> pRegister) <*> pOperand,
      Div <$> (symbol "div" >> pRegister) <*> pOperand,
      Mod <$> (symbol "mod" >> pRegister) <*> pOperand,
      Eql <$> (symbol "eql" >> pRegister) <*> pOperand
    ]

pInp :: Parser [Instruction]
pInp = some (lexemeLn pInstruction)

data AluState = AluState
  { env :: Map Register Value,
    inputs :: [Value]
  }
  deriving (Show)

type ALU = StateT AluState Symbolic

getVal :: Operand -> ALU Value
getVal = \case
  (Reg reg) -> gets ((M.! reg) . env)
  (Val val) -> return val

putVal :: Register -> Value -> ALU ()
putVal reg val = modify (\s -> s {env = M.insert reg val (env s)})

evalInstr :: Instruction -> ALU ()
evalInstr = \case
  (Inp reg) -> do
    v <- lift free_
    putVal reg v
    modify $ \s -> s {inputs = v : inputs s}
  (Add reg op) -> do
    v1 <- getVal (Reg reg)
    v2 <- getVal op
    putVal reg (v1 + v2)
  (Mul reg op) -> do
    v1 <- getVal (Reg reg)
    v2 <- getVal op
    putVal reg (v1 * v2)
  (Div reg op) -> do
    v1 <- getVal (Reg reg)
    v2 <- getVal op
    putVal reg (sDiv v1 v2)
  (Mod reg op) -> do
    v1 <- getVal (Reg reg)
    v2 <- getVal op
    putVal reg (sMod v1 v2)
  (Eql reg op) -> do
    v1 <- getVal (Reg reg)
    v2 <- getVal op
    putVal reg (oneIf (v1 .== v2))

eval :: [Instruction] -> ALU ()
eval = mapM_ evalInstr

run :: ALU () -> Symbolic AluState
run pgm = execStateT pgm initState
  where
    initState =
      AluState
        { env = M.fromList (zip "wxyz" (repeat 0)),
          inputs = []
        }

mkProblem :: (String -> Value -> Symbolic ()) -> ALU () -> Symbolic ()
mkProblem goal pgm = do
  AluState env inputs <- run pgm
  -- z must stop at 0
  constrain $ env M.! 'z' .== 0
  let digits = reverse inputs

  forM_ (zip [14, 13 ..] digits) $ \(i, d) -> do
    constrain $ inRange d (1, 9)

  let modelNum = foldl (\accum digit -> 10 * accum + digit) 0 digits
  goal "goal" modelNum
  modelNumV <- free "model number"
  constrain $ modelNumV .== modelNum

part1 :: [Instruction] -> IO Int64
part1 instrs = do
  let pgm = eval instrs
  let goal = maximize
  LexicographicResult res <- optimize Lexicographic (mkProblem goal pgm)
  return $ fromJust $ getModelValue "model number" res

part2 :: [Instruction] -> IO Int64
part2 instrs = do
  let pgm = eval instrs
  let goal = minimize
  LexicographicResult res <- optimize Lexicographic (mkProblem goal pgm)
  return $ fromJust $ getModelValue "model number" res

main :: IO ()
main = do
  inp <- getInput 24
  instrs <- case parse pInp "day24.input" inp of
    Left err -> putStrLn (errorBundlePretty err) >> exitFailure
    Right ok -> return ok
  putStrLn "Running part 1 ..."
  part1 instrs >>= putAnswer 24 Part1
  putStrLn "Running part 2 ..."
  part2 instrs >>= putAnswer 24 Part2

-- print val
