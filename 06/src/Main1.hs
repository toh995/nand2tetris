module Main1 where

import Control.Exception
import Control.Monad.Except
import Control.Monad.State
import Data.Either
import Data.Functor
import Data.List
import Data.Void
import System.Environment
import System.FilePath
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

newtype ValidationException = ValidationException String
  deriving (Show)
instance Exception ValidationException

type BinaryString = String
type BinaryChar = Char

main :: IO ()
main = do
  filePath <- getArgs <&> head
  ls <- readFile filePath <&> filter (/= ' ') . lines
  print ls

-- instructions <- readFile filePath <&> fromRight [] . runParser instructionsP ""
-- let newFileContents = intercalate "\n" . map instructionToBinary $ instructions
-- let filePath' = replaceExtension filePath ".hack"
-- writeFile filePath' newFileContents

-- print instructions
-- print $ map instructionToBinary instructions

-- main :: IO ()
-- main = do
--   filePath <- getArgs <&> head
--   fileContents <- readFile filePath
--   let filePath' = replaceExtension filePath ".hack"
--   writeFile filePath' fileContents

-- main' :: ExceptT String IO ()
-- main' = do
--   filePath <- ExceptT $ getArgs <&> extractFilePath
--   fileContents <- ExceptT $ readFile filePath <&> Right
--   pure ()
--
-- extractFilePath :: [String] -> Either String FilePath
-- extractFilePath [filePath]
--   | takeExtension filePath == ".asm" = Right filePath
--   | otherwise = Left "asdf"
-- extractFilePath _ = Left "asdf"

-- extractFilePath :: [String] -> ExceptT String IO FilePath
-- extractFilePath [filePath]
--   | takeExtension filePath == ".asm" = pure filePath
--   | otherwise = throwError "asdf"
-- extractFilePath _ = throwError "asdf"

-- extractFilePath :: [String] -> FilePath
-- extractFilePath [filePath]
--   | takeExtension filePath == ".asm" = filePath
--   | otherwise = throw $ ValidationException "asdf"
-- extractFilePath _ = throw $ ValidationException "asdf"

data Instruction
  = AInstruction Int
  | CInstruction
      { dest :: BinaryString
      , comp :: BinaryString
      , jump :: BinaryString
      }
  deriving (Show)

-- CONVERSION
instructionToBinary :: Instruction -> BinaryString
instructionToBinary (AInstruction n) = "0" ++ intToBinary15 n
instructionToBinary CInstruction{comp, dest, jump} = "111" ++ comp ++ dest ++ jump

intToBinary15 :: Int -> BinaryString
intToBinary15 n =
  evalState
    (mapM computeBinChar [14, 13 .. 0])
    (n `mod` 15)
 where
  computeBinChar :: Int -> State Int BinaryChar
  computeBinChar k =
    get >>= \n' ->
      if n' >= (2 ^ k)
        then put (n' - (2 ^ k)) >> pure '1'
        else pure '0'

-- intToBinary15 n = helper (n `mod` 15) 0 ""
--  where
--   helper :: Int -> Int -> BinaryString -> BinaryString
--   helper n' k binStr
--     | k < 0 = binStr
--     | n' >= (2 ^ k) = helper (n' - (2 ^ k)) (k - 1) ('1' : binStr)
--     | otherwise = helper n' (k - 1) ('0' : binStr)

-- foo k = do
--   n' <- get
--   if n' >= (2 ^ k)
--     then pure '1'
--     else pure '0'

-- intToBinary15 :: Int -> BinaryString
-- intToBinary15 n = foldl f "" [14, 13 .. 0]
--   where
--     f :: BinaryString -> Int -> BinaryString
--     f acc k =

-- intToBinary15 n
--   | n >= 2 ^ 15 = Left "foo"
--   | n `mod` 2 == 1 = intToBinary15

-- PARSING
type Parser = Parsec Void String

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
