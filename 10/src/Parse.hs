module Parse (parseTokens, parseClass) where

import Control.Monad.Error.Class
import Data.Functor
import Data.Maybe
import Data.Void
import Debug.Trace
import Text.Megaparsec hiding (Tokens)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import Types

parseTokens :: (MonadError String m) => String -> m AstNode
parseTokens =
  modifyError show
    . liftEither
    . runParser (spaceConsumer *> tokensP) ""

parseClass :: (MonadError String m) => String -> m AstNode
parseClass =
  modifyError show
    . liftEither
    . runParser (spaceConsumer *> p) ""
 where
  p =
    Class <$> do
      nodes1 <-
        sequence
          [ keywordP "class"
          , identifierP
          , symbolP '{'
          ]
      nodes2 <- many classVarDecP
      nodes3 <-
        sequence
          [ keywordP "constructor" <|> keywordP "function" <|> keywordP "method"
          , keywordP "void" <|> typeP
          , identifierP
          , symbolP '('
          ]
      nodes4 <- parameterListP <&> maybeToList
      nodes5 <- sequence [symbolP ')']
      nodes6 <- q
      pure $ nodes1 ++ nodes2 ++ nodes3 ++ nodes4 ++ nodes5 ++ nodes6
  q = do
    node1 <- symbolP '{'
    nodes2 <- many varDecP
    nodes3 <- sequence [keywordP "let", identifierP, symbolP '=', identifierP, symbolP ';']
    pure $ [node1] ++ nodes2 ++ nodes3
  r = do
    node1 <- keywordP "do"
    -- nodes2 <- subroutineCallP
    -- node3 <- symbolP ';'
    -- pure . DoStatement $ [node1] ++ nodes2 ++ [node3]
    pure . DoStatement $ [node1]

-- nodes3 <- many subroutineDecP
-- pure $ nodes1 ++ nodes2 ++ nodes3

-- . runParser (spaceConsumer *> classP) ""

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

spaceConsumer :: Parser ()
spaceConsumer =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

----------------------
-- Primitive Tokens --
----------------------
tokensP :: Parser AstNode
tokensP =
  Tokens
    <$> many tokenP

tokenP :: Parser AstNode
tokenP =
  (choice . map keywordP $ legalKeywords)
    <|> (choice . map symbolP $ legalSymbols)
    <|> integerConstantP
    <|> stringConstantP
    <|> identifierP

keywordP :: String -> Parser AstNode
keywordP s =
  lexeme $
    Keyword <$> string s

symbolP :: Char -> Parser AstNode
symbolP c =
  lexeme $
    Symbol <$> char c

integerConstantP :: Parser AstNode
integerConstantP =
  lexeme $
    IntegerConstant <$> L.decimal

stringConstantP :: Parser AstNode
stringConstantP =
  lexeme $
    StringConstant
      <$> ( char '"'
              *> many (satisfy (/= '"'))
              <* char '"'
          )

identifierP :: Parser AstNode
identifierP = lexeme $ do
  letter <- letterChar
  letters <- many (alphaNumChar <|> char '_')
  pure $ Identifier (letter : letters)

----------------------
-- Composite Tokens --
----------------------
classP :: Parser AstNode
classP = do
  nodes1 <-
    sequence
      [ keywordP "class"
      , identifierP
      , symbolP '{'
      ]
  nodes2 <- many classVarDecP
  nodes3 <- many subroutineDecP
  node4 <- symbolP '}'
  pure . Class $ nodes1 ++ nodes2 ++ nodes3 ++ [node4]

classVarDecP :: Parser AstNode
classVarDecP = do
  nodes1 <-
    sequence
      [ keywordP "static" <|> keywordP "field"
      , typeP
      , identifierP
      ]
  nodes2 <- many (sequence [symbolP ',', identifierP]) <&> concat
  node3 <- symbolP ';'
  pure . ClassVarDec $ nodes1 ++ nodes2 ++ [node3]

typeP :: Parser AstNode
typeP =
  keywordP "int"
    <|> keywordP "char"
    <|> keywordP "boolean"
    <|> identifierP

subroutineDecP :: Parser AstNode
subroutineDecP = do
  nodes1 <-
    sequence
      [ keywordP "constructor" <|> keywordP "function" <|> keywordP "method"
      , keywordP "void" <|> typeP
      , identifierP
      , symbolP '('
      ]
  nodes2 <- parameterListP <&> maybeToList
  nodes3 <- sequence [symbolP ')', subrouteineBodyP]
  pure . SubroutineDec $ nodes1 ++ nodes2 ++ nodes3

parameterListP :: Parser (Maybe AstNode)
parameterListP = optional p
 where
  p = do
    nodes1 <- sequence [typeP, identifierP]
    nodes2 <- many (sequence [symbolP ',', typeP, identifierP]) <&> concat
    pure . ParameterList $ nodes1 ++ nodes2

subrouteineBodyP :: Parser AstNode
subrouteineBodyP =
  do
    node1 <- symbolP '{'
    nodes2 <- many varDecP
    nodes3 <- sequence [statementsP, symbolP '}']
    pure . SubroutineBody $ node1 : nodes2 ++ nodes3

varDecP :: Parser AstNode
varDecP =
  do
    nodes1 <-
      sequence
        [ keywordP "var"
        , typeP
        , identifierP
        ]
    nodes2 <- many (sequence [symbolP ',', identifierP]) <&> concat
    node3 <- symbolP ';'
    pure . VarDec $ nodes1 ++ nodes2 ++ [node3]

statementsP :: Parser AstNode
statementsP = Statements <$> many statementP

statementP :: Parser AstNode
statementP =
  letStatementP
    <|> ifStatementP
    <|> whileStatementP
    <|> doStatementP
    <|> returnStatementP

letStatementP :: Parser AstNode
letStatementP =
  do
    nodes1 <-
      sequence
        [ keywordP "let"
        , identifierP
        ]
    -- nodes2 <- sequence [symbolP '[', expressionP, symbolP ']']
    nodes3 <-
      sequence
        [ symbolP '='
        , expressionP
        , symbolP ';'
        ]
    -- pure . LetStatement $ nodes1 ++ nodes2 ++ nodes3
    pure . LetStatement $ nodes1 ++ nodes3

ifStatementP :: Parser AstNode
ifStatementP =
  do
    nodes1 <-
      sequence
        [ keywordP "if"
        , symbolP '('
        , expressionP
        , symbolP ')'
        , symbolP '{'
        , statementsP
        , symbolP '}'
        ]
    nodes2 <-
      fromMaybe []
        <$> optional
          ( sequence
              [ keywordP "else"
              , symbolP '{'
              , statementsP
              , symbolP '}'
              ]
          )
    pure . IfStatement $ nodes1 ++ nodes2

whileStatementP :: Parser AstNode
whileStatementP =
  WhileStatement
    <$> sequence
      [ keywordP "while"
      , symbolP '('
      , expressionP
      , symbolP ')'
      , symbolP '{'
      , statementsP
      , symbolP '}'
      ]

doStatementP :: Parser AstNode
doStatementP = do
  node1 <- keywordP "do"
  nodes2 <- subroutineCallP
  node3 <- symbolP ';'
  pure . DoStatement $ [node1] ++ nodes2 ++ [node3]

returnStatementP :: Parser AstNode
returnStatementP = do
  node1 <- keywordP "return"
  nodes2 <- optional expressionP <&> maybeToList
  node3 <- symbolP ';'
  pure . ReturnStatement $ [node1] ++ nodes2 ++ [node3]

expressionP :: Parser AstNode
expressionP = identifierP <&> \i -> Expression [i]

-- Expression <$> sequence [termP]

termP :: Parser AstNode
termP = identifierP

subroutineCallP :: Parser [AstNode]
subroutineCallP = try p1 <|> try p2
 where
  p1 = do
    nodes1 <- sequence [identifierP, symbolP '(']
    nodes2 <- expressionListP <&> maybeToList
    node3 <- symbolP ')'
    pure $ nodes1 ++ nodes2 ++ [node3]
  p2 = do
    nodes1 <-
      sequence
        [ identifierP
        , symbolP '.'
        , identifierP
        , symbolP '('
        ]
    nodes2 <- expressionListP <&> maybeToList
    node3 <- symbolP ')'
    pure $ nodes1 ++ nodes2 ++ [node3]

expressionListP :: Parser (Maybe AstNode)
expressionListP = try $ optional p
 where
  p = do
    node1 <- expressionP
    nodes2 <- many (sequence [symbolP ',', expressionP]) <&> concat
    pure . ExpressionList $ node1 : nodes2
