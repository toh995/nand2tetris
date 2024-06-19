module Pipee where

import Control.Applicative
import Control.Monad.Except
import Pipes

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

data AstNode
  = TokenNode Token
  | CompositeNode NodeType [AstNode]

keyword :: (MonadError String m) => String -> Consumer Token m AstNode
keyword s =
  await >>= \token ->
    if token == Keyword s
      then pure $ TokenNode (Keyword s)
      else throwError "asdfasd"

symbol :: (MonadError String m) => Char -> Consumer Token m AstNode
symbol c =
  await >>= \token ->
    if token == Symbol c
      then pure $ TokenNode (Symbol c)
      else throwError "asdfasd"

integerConstant :: (MonadError String m) => Consumer Token m AstNode
integerConstant =
  await >>= \case
    IntegerConstant x -> pure $ TokenNode (IntegerConstant x)
    _ -> throwError "asdfasdf"

stringConstant :: (MonadError String m) => Consumer Token m AstNode
stringConstant =
  await >>= \case
    StringConstant x -> pure $ TokenNode (StringConstant x)
    _ -> throwError "asdfasdf"

identifier :: (MonadError String m) => Consumer Token m AstNode
identifier =
  await >>= \case
    Identifier x -> pure $ TokenNode (Identifier x)
    _ -> throwError "asdfasdf"

classNode :: (MonadError String m) => Consumer Token m AstNode
classNode =
  CompositeNode Class
    <$> sequence
      [ keyword "class"
      , identifier
      , symbol '{'
      ]

-- classVarDec :: (Alternative m, MonadError String m) => Consumer Token m AstNode
-- classVarDec =
--   CompositeNode ClassVarDec
--     <$> sequence
--       [ keyword "static" <|> keyword "field"
--       , -- , type
--         identifier
--       ]
