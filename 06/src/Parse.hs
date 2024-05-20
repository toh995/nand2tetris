module Parse where

import Data.Either
import Data.Functor
import Data.Void
import Text.Megaparsec hiding (Label, State, label)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import Types

type Parser = Parsec Void String

parseLines :: String -> [Line]
parseLines = fromRight [] . runParser linesP ""

ignoreP :: Parser ()
ignoreP =
  L.space
    space1
    (L.skipLineComment "//")
    empty

linesP :: Parser [Line]
linesP =
  ignoreP
    *> lineP `sepEndBy` ignoreP

lineP :: Parser Line
lineP = aInstructionP <|> cInstructionP <|> labelP

labelP :: Parser Line
labelP = do
  _ <- char '('
  labelName <- takeWhile1P Nothing (/= ')')
  _ <- char ')'
  pure $ LabelLine labelName

aInstructionP :: Parser Line
aInstructionP =
  AInstructionLine
    <$> ( char '@'
            *> takeWhile1P Nothing (not . (`elem` ['\n', '\r']))
        )

cInstructionP :: Parser Line
cInstructionP = do
  dest <- try (destBinStrP <* char '=') <|> pure "000"
  comp <- compBinStrP
  jump <- try (char ';' *> jumpBinStrP) <|> pure "000"
  pure CInstructionLine{dest, comp, jump}

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

jumpBinStrP :: Parser BinaryString
jumpBinStrP =
  (string "null" $> "000")
    <|> (string "JGT" $> "001")
    <|> (string "JEQ" $> "010")
    <|> (string "JGE" $> "011")
    <|> (string "JLT" $> "100")
    <|> (string "JNE" $> "101")
    <|> (string "JLE" $> "110")
    <|> (string "JMP" $> "111")

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
