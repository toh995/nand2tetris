module Parse (parseCommands) where

import Control.Monad.Error.Class
import Data.Functor
import Data.Void
import Text.Megaparsec hiding (Label)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import Types

parseCommands :: (MonadError String m) => String -> m [VmCommand]
parseCommands =
  modifyError show
    . liftEither
    . runParser commandsP ""

type Parser = Parsec Void String

commandsP :: Parser [VmCommand]
commandsP =
  ignoreP
    *> commandP `sepEndBy` ignoreP

ignoreP :: Parser ()
ignoreP =
  L.space
    space1
    (L.skipLineComment "//")
    empty

commandP :: Parser VmCommand
commandP =
  (string "add" $> Arithmetic Add)
    <|> (string "sub" $> Arithmetic Sub)
    <|> (string "neg" $> Arithmetic Neg)
    <|> (string "eq" $> Logical Eq)
    <|> (string "gt" $> Logical Gt)
    <|> (string "lt" $> Logical Lt)
    <|> (string "and" $> Logical And)
    <|> (string "or" $> Logical Or)
    <|> (string "not" $> Logical Not)
    <|> ( do
            _ <- string "push "
            segment <- segmentP <* char ' '
            Memory . Push segment <$> L.decimal
        )
    <|> ( do
            _ <- string "pop "
            segment <- segmentP <* char ' '
            Memory . Pop segment <$> L.decimal
        )

segmentP :: Parser Segment
segmentP =
  (string "local" $> Local)
    <|> (string "argument" $> Argument)
    <|> (string "this" $> This)
    <|> (string "that" $> That)
    <|> (string "constant" $> Constant)
    <|> (string "static" $> Static)
    <|> (string "temp" $> Temp)
    <|> (string "pointer" $> Pointer)
