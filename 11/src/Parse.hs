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
      *> many (satisfy ((&&) <$> (/= '"') <*> (/= '\n')))
      <* char '"'

identifierP :: Parser String
identifierP = lexeme $
  do
    headChar <- letterChar <|> char '_'
    tailChars <- many (letterChar <|> char '_' <|> digitChar)
    pure $ headChar : tailChars

-- Composite
classP :: Parser Class
classP = do
  _ <- keywordP "class"
  name <- identifierP
  _ <- symbolP '{'
  classVars <- concat <$> many classVarDecP
  subroutineDecs <- many subroutineDecP
  _ <- symbolP '}'
  pure Class{name, classVars, subroutineDecs}

classVarDecP :: Parser [Variable]
classVarDecP = do
  kind <-
    (keywordP "static" $> StaticVar)
      <|> (keywordP "field" $> FieldVar)
  type' <- typeP
  names <- sepBy1 identifierP (symbolP ',')
  _ <- symbolP ';'
  let vars =
        map
          (\name -> Variable{name, type', kind, index = 0})
          names
  pure vars

subroutineDecP :: Parser SubroutineDec
subroutineDecP = do
  kind <-
    (keywordP "constructor" $> ConstructorKind)
      <|> (keywordP "function" $> FunctionKind)
      <|> (keywordP "method" $> MethodKind)
  _ <- keywordP "void" <|> typeP
  name <- identifierP
  _ <- symbolP '('
  args <- sepBy argDecP (symbolP ',')
  _ <- symbolP ')' >> symbolP '{'
  localVars <- concat <$> many localVarsP
  statements <- many statementP
  _ <- symbolP '}'
  pure SubroutineDec{name, args, localVars, statements, kind}

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
  letStatementP
    <|> letStatementArrayP
    <|> ifStatementP
    <|> whileStatementP
    <|> doStatementP
    <|> returnStatementP

letStatementP :: Parser Statement
letStatementP = try $ do
  _ <- keywordP "let"
  varName <- identifierP
  _ <- symbolP '='
  expr <- exprP
  _ <- symbolP ';'
  pure $ LetStatement varName expr

letStatementArrayP :: Parser Statement
letStatementArrayP = do
  _ <- keywordP "let"
  varName <- identifierP
  _ <- symbolP '['
  indexExpr <- exprP
  _ <- symbolP ']'
  _ <- symbolP '='
  rhsExpr <- exprP
  _ <- symbolP ';'
  pure $ LetStatementArray{varName, indexExpr, rhsExpr}

ifStatementP :: Parser Statement
ifStatementP = do
  _ <- keywordP "if"
  _ <- symbolP '('
  expr <- exprP
  _ <- symbolP ')'
  _ <- symbolP '{'
  ifStatements <- many statementP
  _ <- symbolP '}'
  elseStatements <- elseStatementsP <|> pure []
  pure IfStatement{expr, ifStatements, elseStatements}

elseStatementsP :: Parser [Statement]
elseStatementsP = do
  _ <- keywordP "else"
  _ <- symbolP '{'
  ret <- many statementP
  _ <- symbolP '}'
  pure ret

whileStatementP :: Parser Statement
whileStatementP = do
  _ <- keywordP "while"
  _ <- symbolP '('
  expr <- exprP
  _ <- symbolP ')'
  _ <- symbolP '{'
  statements <- many statementP
  _ <- symbolP '}'
  pure WhileStatement{expr, statements}

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
subroutineCallP = simpleSubroutineCallP <|> compoundSubroutineCallP

simpleSubroutineCallP :: Parser SubroutineCall
simpleSubroutineCallP = try $ do
  name <- identifierP
  _ <- symbolP '('
  exprs <- sepBy exprP (symbolP ',')
  _ <- symbolP ')'
  pure SimpleSubroutineCall{name, exprs}

compoundSubroutineCallP :: Parser SubroutineCall
compoundSubroutineCallP = try $ do
  leftName <- identifierP
  _ <- symbolP '.'
  rightName <- identifierP
  _ <- symbolP '('
  exprs <- sepBy exprP (symbolP ',')
  _ <- symbolP ')'
  pure CompoundSubroutineCall{leftName, rightName, exprs}

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
  (IntLiteral <$> integerConstantP)
    <|> (StringLiteral <$> stringConstantP)
    <|> (TrueLiteral <$ keywordP "true")
    <|> (FalseLiteral <$ keywordP "false")
    <|> (NullLiteral <$ keywordP "null")
    <|> (ThisKeyword <$ keywordP "this")
    <|> (SubroutineCallTerm <$> subroutineCallP)
    <|> try
      ( do
          varName <- identifierP
          _ <- symbolP '['
          indexExpr <- exprP
          _ <- symbolP ']'
          pure $ ArrayAccessTerm{varName, indexExpr}
      )
    <|> (VarTerm <$> identifierP)
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
    <|> (symbolP '-' $> AstSub)
    <|> (symbolP '*' $> AstMul)
    <|> (symbolP '/' $> AstDiv)
    <|> (symbolP '&' $> AstAnd)
    <|> (symbolP '|' $> AstOr)
    <|> (symbolP '<' $> AstLt)
    <|> (symbolP '>' $> AstGt)
    <|> (symbolP '=' $> AstEq)
