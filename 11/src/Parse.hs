module Parse (parseClass) where

import Control.Monad.Except
import Data.Functor
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import AST

parseClass :: (MonadError String m) => String -> m Class
parseClass =
  modifyError show
    . liftEither
    . runParser (ignoreP *> classP) ""

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = L.lexeme ignoreP

ignoreP :: Parser ()
ignoreP =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

keywordP :: String -> Parser String
keywordP = lexeme . string

symbolP :: Char -> Parser Char
symbolP = lexeme . char

integerConstantP :: Parser Int
integerConstantP = lexeme L.decimal

stringConstantP :: Parser String
stringConstantP =
  lexeme $
    char '"'
      *> many (satisfy (/= '"'))
      <* char '"'

identifierP :: Parser String
identifierP = lexeme $
  do
    headChar <- letterChar
    tailChars <- many (alphaNumChar <|> char '_')
    pure $ headChar : tailChars

-- Composite
classP :: Parser Class
classP = do
  _ <- keywordP "class"
  name <- identifierP
  _ <- symbolP '{'
  subroutineDecs <- many $ subroutineDecP name
  pure Class{name, subroutineDecs, classVars = []}

subroutineDecP :: String -> Parser SubroutineDec
subroutineDecP parentClassName = do
  _ <- keywordP "function"
  _ <- keywordP "void" <|> typeP
  name <- identifierP
  _ <- symbolP '('
  args <- sepBy argDecP (symbolP ',')
  _ <- symbolP ')' >> symbolP '{'
  localVars <- concat <$> many localVarsP
  statements <- many statementP
  _ <- symbolP '}'
  pure SubroutineDec{name, args, localVars, statements, parentClassName}

typeP :: Parser String
typeP =
  choice (keywordP <$> ["int", "char", "boolean"])
    <|> identifierP

argDecP :: Parser Variable
argDecP = do
  type' <- typeP
  name <- identifierP
  pure Variable{name, type', kind = ArgVar, index = 0}

localVarsP :: Parser [Variable]
localVarsP = do
  _ <- keywordP "var"
  type' <- typeP
  varNames <- sepBy identifierP (symbolP ',')
  _ <- symbolP ';'
  pure $
    map
      (\name -> Variable{name, type', kind = LocalVar, index = 0})
      varNames

statementP :: Parser Statement
statementP =
  doStatementP
    <|> returnStatementP

doStatementP :: Parser Statement
doStatementP = do
  _ <- keywordP "do"
  call <- subroutineCallP
  _ <- symbolP ';'
  pure $ DoStatement call

returnStatementP :: Parser Statement
returnStatementP = do
  _ <- keywordP "return"
  maybeExpr <- optional exprP
  _ <- symbolP ';'
  pure $ ReturnStatement maybeExpr

subroutineCallP :: Parser SubroutineCall
subroutineCallP = do
  name1 <- identifierP
  _ <- symbolP '.'
  name2 <- identifierP
  _ <- symbolP '('
  exprs <- sepBy exprP (symbolP ',')
  _ <- symbolP ')'
  pure SubroutineCall{name = name1 ++ "." ++ name2, exprs}

exprP :: Parser Expr
exprP = try binaryExprP <|> singleExprP

binaryExprP :: Parser Expr
binaryExprP =
  BinaryExpr
    <$> termP
    <*> binaryOpP
    <*> exprP

singleExprP :: Parser Expr
singleExprP = SingleExpr <$> termP

termP :: Parser Term
termP =
  (IntLiteralTerm <$> integerConstantP)
    <|> ( do
            _ <- symbolP '('
            expr <- exprP
            _ <- symbolP ')'
            pure $ ExprTerm expr
        )
    <|> (UnaryOpTerm <$> unaryOpP <*> termP)

unaryOpP :: Parser UnaryOp
unaryOpP =
  (symbolP '-' $> AstNeg)
    <|> (symbolP '~' $> AstNot)

binaryOpP :: Parser BinaryOp
binaryOpP =
  (symbolP '+' $> AstAdd)
    <|> (symbolP '*' $> AstMult)