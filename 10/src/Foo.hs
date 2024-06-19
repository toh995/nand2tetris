module Foo where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Either
import Data.Functor
import Data.Maybe
import Text.Megaparsec (option, unPos)

data Token
  = Keyword String
  | Symbol Char
  | IntegerConstant Int
  | StringConstant String
  | Identifier String
  deriving (Eq, Show)

data NodeType
  = Tokens
  | Class
  | ClassVarDec
  | SubroutineDec
  | ParameterList
  | SubroutineBody
  | VarDec
  | Statements
  | LetStatement
  | IfStatement
  | WhileStatement
  | DoStatement
  | ReturnStatement
  | Expression
  | Term
  | ExpressionList

data AstNode
  = TokenNode Token
  | CompositeNode NodeType [AstNode]

peekOrThrow :: (MonadState [l] m, MonadError String m) => m l
peekOrThrow =
  get >>= \case
    (l : _) -> pure l
    [] -> throwError "asdf"

advance :: (MonadState [l] m, MonadError String m) => m ()
advance =
  modify $ \case
    (_ : ls) -> ls
    [] -> []

keyword :: (MonadState [Token] m, MonadError String m) => String -> m AstNode
keyword s =
  peekOrThrow
    >>= ( \token ->
            if token == Keyword s
              then TokenNode (Keyword s) <$ advance
              else
                throwError "asdfasd"
        )

symbol :: (MonadState [Token] m, MonadError String m) => Char -> m AstNode
symbol c =
  peekOrThrow >>= \token ->
    if token == Symbol c
      then TokenNode (Symbol c) <$ advance
      else throwError "asdfasd"

integerConstant :: (MonadState [Token] m, MonadError String m) => m AstNode
integerConstant =
  peekOrThrow >>= \case
    IntegerConstant x -> TokenNode (IntegerConstant x) <$ advance
    _ -> throwError "asdfasdf"

stringConstant :: (MonadState [Token] m, MonadError String m) => m AstNode
stringConstant =
  peekOrThrow >>= \case
    StringConstant x -> TokenNode (StringConstant x) <$ advance
    _ -> throwError "asdfasdf"

identifier :: (MonadState [Token] m, MonadError String m) => m AstNode
identifier =
  peekOrThrow >>= \case
    Identifier x -> TokenNode (Identifier x) <$ advance
    _ -> throwError "asdfasdf"

classNode :: (MonadState [Token] m, MonadError String m, Alternative m) => m AstNode
classNode =
  CompositeNode Class
    <$> do
      nodes1 <-
        sequence
          [ keyword "class"
          , identifier
          , symbol '{'
          ]
      nodes2 <- many classVarDecNode
      nodes3 <- many subroutineDecNode
      node4 <- symbol '}'
      pure $ nodes1 ++ nodes2 ++ nodes3 ++ [node4]

classVarDecNode :: (MonadState [Token] m, MonadError String m, Alternative m) => m AstNode
classVarDecNode =
  CompositeNode ClassVarDec
    <$> do
      nodes1 <-
        sequence
          [ keyword "static" <|> keyword "field"
          , typeNode
          , identifier
          ]
      nodes2 <- many (sequence [symbol ',', identifier]) <&> concat
      node3 <- symbol ';'
      pure $ nodes1 ++ nodes2 ++ [node3]

typeNode :: (MonadState [Token] m, MonadError String m, Alternative m) => m AstNode
typeNode =
  keyword "int"
    <|> keyword "char"
    <|> keyword "boolean"
    <|> identifier

subroutineDecNode :: (MonadState [Token] m, MonadError String m, Alternative m) => m AstNode
subroutineDecNode =
  CompositeNode SubroutineDec
    <$> do
      nodes1 <-
        sequence
          [ keyword "constructor" <|> keyword "function" <|> keyword "method"
          , keyword "void" <|> typeNode
          , identifier
          , symbol '('
          ]
      nodes2 <- parameterListNode <&> maybeToList
      nodes3 <- sequence [symbol ')', subroutineBodyNode]
      pure $ nodes1 ++ nodes2 ++ nodes3

parameterListNode :: (MonadState [Token] m, MonadError String m, Alternative m) => m (Maybe AstNode)
parameterListNode = optional p
 where
  p = do
    nodes1 <- sequence [typeNode, identifier]
    nodes2 <- many (sequence [symbol ',', typeNode, identifier]) <&> concat
    pure . CompositeNode ParameterList $ nodes1 ++ nodes2

subroutineBodyNode :: (MonadState [Token] m, MonadError String m, Alternative m) => m AstNode
subroutineBodyNode =
  CompositeNode SubroutineBody
    <$> do
      node1 <- symbol '{'
      nodes2 <- many varDecNode
      nodes3 <- sequence [statementsNode, symbol '}']
      pure $ node1 : nodes2 ++ nodes3

varDecNode :: (MonadState [Token] m, MonadError String m, Alternative m) => m AstNode
varDecNode =
  CompositeNode VarDec
    <$> do
      nodes1 <-
        sequence
          [ keyword "var"
          , typeNode
          , identifier
          ]
      nodes2 <- many (sequence [symbol ',', identifier]) <&> concat
      node3 <- symbol ';'
      pure $ nodes1 ++ nodes2 ++ [node3]

statementsNode :: (MonadState [Token] m, MonadError String m, Alternative m) => m AstNode
statementsNode = CompositeNode Statements <$> many statementNode

statementNode :: (MonadState [Token] m, MonadError String m, Alternative m) => m AstNode
statementNode =
  letStatementNode
    <|> ifStatementNode
    <|> whileStatementNode
    <|> doStatementNode
    <|> returnStatementNode

letStatementNode :: (MonadState [Token] m, MonadError String m, Alternative m) => m AstNode
letStatementNode =
  CompositeNode LetStatement
    <$> do
      nodes1 <-
        sequence
          [ keyword "let"
          , identifier
          ]
      nodes2 <-
        fromMaybe []
          <$> optional
            ( sequence
                [ symbol '['
                , expressionNode
                , symbol ']'
                ]
            )
      nodes3 <-
        sequence
          [ symbol '='
          , expressionNode
          , symbol ';'
          ]
      pure $ nodes1 ++ nodes2 ++ nodes3

ifStatementNode :: (MonadState [Token] m, MonadError String m, Alternative m) => m AstNode
ifStatementNode =
  CompositeNode IfStatement
    <$> do
      nodes1 <-
        sequence
          [ keyword "if"
          , symbol '('
          , expressionNode
          , symbol ')'
          , symbol '{'
          , statementsNode
          , symbol '}'
          ]
      nodes2 <-
        fromMaybe []
          <$> optional
            ( sequence
                [ keyword "else"
                , symbol '{'
                , statementsNode
                , symbol '}'
                ]
            )
      pure $ nodes1 ++ nodes2

whileStatementNode :: (MonadState [Token] m, MonadError String m, Alternative m) => m AstNode
whileStatementNode =
  CompositeNode WhileStatement
    <$> sequence
      [ keyword "while"
      , symbol '('
      , expressionNode
      , symbol ')'
      , symbol '{'
      , statementsNode
      , symbol '}'
      ]

doStatementNode :: (MonadState [Token] m, MonadError String m, Alternative m) => m AstNode
doStatementNode =
  CompositeNode DoStatement
    <$> do
      node1 <- keyword "do"
      nodes2 <- subroutineCallNode
      node3 <- symbol ';'
      pure $ [node1] ++ nodes2 ++ [node3]

returnStatementNode :: (MonadState [Token] m, MonadError String m, Alternative m) => m AstNode
returnStatementNode =
  CompositeNode ReturnStatement
    <$> do
      node1 <- keyword "return"
      nodes2 <- optional expressionNode <&> maybeToList
      node3 <- symbol ';'
      pure $ [node1] ++ nodes2 ++ [node3]

expressionNode :: (MonadState [Token] m, MonadError String m, Alternative m) => m AstNode
expressionNode =
  CompositeNode Expression
    <$> do
      node1 <- termNode
      nodes2 <- many (sequence [opNode, termNode]) <&> concat
      pure $ node1 : nodes2

termNode :: (MonadState [Token] m, MonadError String m, Alternative m) => m AstNode
termNode = CompositeNode Term <$> p
 where
  p =
    sequence [integerConstant]
      <|> sequence [stringConstant]
      <|> sequence [keywordConstantNode]
      <|> sequence [unaryOpNode, termNode]
      <|> sequence [symbol '(', expressionNode, symbol ')']
      <|> ( peekOrThrow
              >>= \case
                Identifier _ ->
                  subroutineCallNode
                    <|> sequence [identifier, symbol '[', expressionNode, symbol ']']
                    <|> sequence [identifier]
                _ -> throwError "asdf"
          )

subroutineCallNode :: (MonadState [Token] m, MonadError String m, Alternative m) => m [AstNode]
subroutineCallNode = p1 <|> p2
 where
  p1 = do
    nodes1 <-
      sequence
        [ identifier
        , symbol '('
        ]
    nodes2 <- expressionListNode <&> maybeToList
    node3 <- symbol ')'
    pure $ nodes1 ++ nodes2 ++ [node3]
  p2 = do
    nodes1 <-
      sequence
        [ identifier
        , symbol '.'
        , identifier
        , symbol '('
        ]
    nodes2 <- expressionListNode <&> maybeToList
    node3 <- symbol ')'
    pure $ nodes1 ++ nodes2 ++ [node3]

expressionListNode :: (MonadState [Token] m, MonadError String m, Alternative m) => m (Maybe AstNode)
expressionListNode = optional p
 where
  p = do
    node1 <- expressionNode
    nodes2 <- many (sequence [symbol ',', expressionNode]) <&> concat
    pure . CompositeNode ExpressionList $ node1 : nodes2

opNode :: (MonadState [Token] m, MonadError String m, Alternative m) => m AstNode
opNode =
  symbol '+'
    <|> symbol '-'
    <|> symbol '*'
    <|> symbol '/'
    <|> symbol '&'
    <|> symbol '|'
    <|> symbol '<'
    <|> symbol '>'
    <|> symbol '='

unaryOpNode :: (MonadState [Token] m, MonadError String m, Alternative m) => m AstNode
unaryOpNode = symbol '-' <|> symbol '~'

keywordConstantNode :: (MonadState [Token] m, MonadError String m, Alternative m) => m AstNode
keywordConstantNode =
  keyword "true"
    <|> keyword "false"
    <|> keyword "null"
    <|> keyword "this"
