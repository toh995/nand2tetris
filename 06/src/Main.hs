module Main where

import Control.Monad.State
import Data.Either
import Data.Functor
import Data.HashMap.Lazy qualified as HashMap
import Data.Void
import System.Environment
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

main :: IO ()
main = do
  filePath <- getArgs <&> head
  -- instructions <- readFile filePath <&> fromRight [] . runParser instructionsP ""
  pure ()

type BinaryString = String
type BinaryChar = Char

data Instruction
  = AInstruction Int
  | CInstruction
      { dest :: BinaryString
      , comp :: BinaryString
      , jump :: BinaryString
      }
  deriving (Show)

type Parser = ParsecT Void String (State S)

type LabelName = String
data S = S
  { pc :: Int
  , labelMap :: HashMap.HashMap LabelName Int
  , instructions :: [Instruction]
  }

ignoreP :: Parser ()
ignoreP =
  L.space
    space1
    (L.skipLineComment "//")
    empty

instructionsP :: Parser [Instruction]
instructionsP =
  ignoreP
    *> instructionP `sepEndBy` ignoreP

instructionP :: Parser Instruction
instructionP = aInstructionP <|> cInstructionP

aInstructionP :: Parser Instruction
aInstructionP =
  AInstruction
    <$> (char '@' *> L.decimal)

cInstructionP :: Parser Instruction
cInstructionP = do
  dest <- destBinStrP <* char '='
  comp <- compBinStrP
  let jump = "000"
  pure CInstruction{dest, comp, jump}

destBinStrP :: Parser BinaryString
destBinStrP =
  -- Order the "primitive" expressions LAST, so that they
  -- don't conflict with the "compound" expressions
  (string "null" $> "000")
    <|> (string "AMD" $> "111")
    <|> (string "MD" $> "011")
    <|> (string "AM" $> "101")
    <|> (string "AD" $> "110")
    <|> (string "A" $> "100")
    <|> (string "M" $> "001")
    <|> (string "D" $> "010")

compBinStrP :: Parser BinaryString
compBinStrP =
  -- Order the "primitive" expressions LAST, so that they
  -- don't conflict with the "compound" expressions
  (string "D+1" $> "0011111")
    <|> (string "A+1" $> "0110111")
    <|> (string "D-1" $> "0001110")
    <|> (string "A-1" $> "0110010")
    <|> (string "D+A" $> "0000010")
    <|> (string "D-A" $> "0010011")
    <|> (string "A-D" $> "0000111")
    <|> (string "D&A" $> "0000000")
    <|> (string "D|A" $> "0010101")
    <|> (string "M+1" $> "1110111")
    <|> (string "M-1" $> "1110010")
    <|> (string "D+M" $> "1000010")
    <|> (string "D-M" $> "1010011")
    <|> (string "M-D" $> "1000111")
    <|> (string "D&M" $> "1000000")
    <|> (string "D|M" $> "1010101")
    <|> (string "!M" $> "1110001")
    <|> (string "-M" $> "1110011")
    <|> (string "-1" $> "0111010")
    <|> (string "!D" $> "0001101")
    <|> (string "!A" $> "0110001")
    <|> (string "-D" $> "0001111")
    <|> (string "-A" $> "0110011")
    <|> (string "0" $> "0101010")
    <|> (string "1" $> "0111111")
    <|> (string "D" $> "0001100")
    <|> (string "A" $> "0110000")
    <|> (string "M" $> "1110000")
