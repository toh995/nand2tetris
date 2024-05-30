module Parse (parseVmCommands) where

import Control.Monad.Error.Class
import Data.Functor
import Data.Void
import Text.Megaparsec hiding (Label)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import Types

parseVmCommands :: (MonadError String m) => String -> m [VmCommand]
parseVmCommands =
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
  -- Arithmetic
  (string "add" $> Arithmetic Add)
    <|> (string "sub" $> Arithmetic Sub)
    <|> (string "neg" $> Arithmetic Neg)
    -- Branching
    <|> ( string "label "
            *> someTill anySingle (space1 <|> L.skipLineComment "//")
            <&> Branching . Label
        )
    <|> ( string "goto "
            *> someTill anySingle (space1 <|> L.skipLineComment "//")
            <&> Branching . Goto
        )
    <|> ( string "if-goto "
            *> someTill anySingle (space1 <|> L.skipLineComment "//")
            <&> Branching . IfGoto
        )
    -- Function
    <|> (string "return" $> Function Return)
    <|> ( do
            _ <- string "function "
            functionName <- takeWhile1P (Just "functionName") (/= ' ') <* string " "
            numVars <- L.decimal
            pure . Function $ FunctionDef{functionName, numVars}
        )
    <|> ( do
            _ <- string "call "
            functionName <- takeWhile1P (Just "functionName") (/= ' ') <* string " "
            numArgs <- L.decimal
            pure . Function $ FunctionCall{functionName, numArgs}
        )
    -- Logical
    <|> (string "eq" $> Logical Eq)
    <|> (string "gt" $> Logical Gt)
    <|> (string "lt" $> Logical Lt)
    <|> (string "and" $> Logical And)
    <|> (string "or" $> Logical Or)
    <|> (string "not" $> Logical Not)
    -- Memory
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
