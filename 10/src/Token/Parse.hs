module Token.Parse (parseTokenList) where

import Control.Monad.Except
import Data.Void
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import Token

parseTokenList :: (MonadError String m) => String -> m [Token]
parseTokenList =
  modifyError show
    . liftEither
    . runParser (ignoreP *> tokensP) ""

type Parser = Parsec Void String

ignoreP :: Parser ()
ignoreP =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

tokensP :: Parser [Token]
tokensP = sepEndBy tokenP ignoreP

tokenP :: Parser Token
tokenP =
  keywordP
    <|> symbolP
    <|> integerConstantP
    <|> stringConstantP
    <|> identifierP

keywordP :: Parser Token
keywordP =
  Keyword
    <$> (choice . map string $ legalKeywords)

symbolP :: Parser Token
symbolP = Symbol <$> oneOf legalSymbols

integerConstantP :: Parser Token
integerConstantP = IntegerConstant <$> L.decimal

stringConstantP :: Parser Token
stringConstantP =
  StringConstant
    <$> ( char '"'
            *> many (satisfy (/= '"'))
            <* char '"'
        )

identifierP :: Parser Token
identifierP = do
  headChar <- letterChar
  tailChars <- many (alphaNumChar <|> char '_')
  pure $ Identifier (headChar : tailChars)
