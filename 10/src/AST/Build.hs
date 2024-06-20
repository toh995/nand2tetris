module AST.Build (buildTokensAST, buildClassAST) where

import Control.Monad.Except
import Control.Monad.State
import Data.Functor
import Data.List
import Data.Maybe

import AST
import Token

buildTokensAST :: (MonadError String m) => [Token] -> m AstNode
buildTokensAST = evalStateT tokensNode

buildClassAST :: (MonadError String m) => [Token] -> m AstNode
buildClassAST = evalStateT classNode

------------------
-- Helper utils --
------------------
oneOf :: (Foldable t, MonadError e m) => t (m a) -> m a
oneOf = foldr1 f
 where
  f a1 a2 = catchError a1 (const a2)

many :: (MonadError e m) => m a -> m [a]
many m =
  tryError m >>= \case
    Right a -> (:) a <$> many m
    Left _ -> pure []

optional :: (MonadError e m) => m a -> m (Maybe a)
optional m =
  tryError m <&> \case
    Right a -> Just a
    Left _ -> Nothing

concatM :: (Monad m) => [m [a]] -> m [a]
concatM ms = sequence ms <&> concat

assertEqual :: (MonadError String m, Eq a, Show a) => a -> a -> m ()
assertEqual expected actual
  | expected == actual = pure ()
  | otherwise =
      throwError $
        "Expected value '" ++ show expected ++ "', instead got '" ++ show actual ++ "'"

assertIsIdentifier :: (MonadError String m) => Token -> m ()
assertIsIdentifier (Identifier _) = pure ()
assertIsIdentifier token =
  throwError $
    "Expected a token of type 'IntegerConstant', instead got " ++ show token

peekHeadOrThrow :: (MonadState [l] m, MonadError String m) => m l
peekHeadOrThrow =
  get >>= \case
    (l : _) -> pure l
    [] -> throwError "Could not peek the head; reached the end of the list!"

peekNextOrThrow :: (MonadState [l] m, MonadError String m) => m l
peekNextOrThrow =
  get >>= \case
    (_ : l : _) -> pure l
    _ -> throwError "Could not peek next; reached the end of the list!"

advance :: (MonadState [l] m, MonadError String m) => m ()
advance =
  modify $ \case
    (_ : ls) -> ls
    [] -> []

--------------------
-- AST Leaf Nodes --
--------------------
keyword :: (MonadState [Token] m, MonadError String m) => String -> m AstNode
keyword s =
  peekHeadOrThrow >>= \token ->
    assertEqual (Keyword s) token
      >> advance
        $> LeafNode (Keyword s)

symbol :: (MonadState [Token] m, MonadError String m) => Char -> m AstNode
symbol c =
  peekHeadOrThrow >>= \token ->
    assertEqual (Symbol c) token
      >> advance
        $> LeafNode (Symbol c)

integerConstant :: (MonadState [Token] m, MonadError String m) => m AstNode
integerConstant =
  peekHeadOrThrow >>= \case
    IntegerConstant x ->
      advance
        $> LeafNode (IntegerConstant x)
    token ->
      throwError $
        "Expected a token of type 'IntegerConstant', instead got " ++ show token

stringConstant :: (MonadState [Token] m, MonadError String m) => m AstNode
stringConstant =
  peekHeadOrThrow >>= \case
    StringConstant x ->
      advance
        $> LeafNode (StringConstant x)
    token ->
      throwError $
        "Expected a token of type 'StringConstant', instead got " ++ show token

identifier :: (MonadState [Token] m, MonadError String m) => m AstNode
identifier =
  peekHeadOrThrow >>= \case
    Identifier x ->
      advance
        $> LeafNode (Identifier x)
    token ->
      throwError $
        "Expected a token of type 'Identifier, instead got " ++ show token

----------------------
-- AST Branch Nodes --
----------------------
tokensNode :: (MonadState [Token] m, MonadError String m) => m AstNode
tokensNode = BranchNode Tokens <$> many tokenNode

tokenNode :: (MonadState [Token] m, MonadError String m) => m AstNode
tokenNode =
  oneOf
    [ oneOf $ map keyword legalKeywords
    , oneOf $ map symbol legalSymbols
    , integerConstant
    , stringConstant
    , identifier
    ]

classNode :: (MonadState [Token] m, MonadError String m) => m AstNode
classNode =
  BranchNode Class
    <$> concatM
      [ sequence
          [ keyword "class"
          , identifier
          , symbol '{'
          ]
      , many classVarDecNode
      , many subroutineDecNode
      , symbol '}' <&> singleton
      ]

classVarDecNode :: (MonadState [Token] m, MonadError String m) => m AstNode
classVarDecNode =
  BranchNode ClassVarDec
    <$> concatM
      [ sequence
          [ oneOf $ keyword <$> ["static", "field"]
          , typeNode
          , identifier
          ]
      , many (sequence [symbol ',', identifier]) <&> concat
      , symbol ';' <&> singleton
      ]

typeNode :: (MonadState [Token] m, MonadError String m) => m AstNode
typeNode =
  oneOf
    [ keyword "int"
    , keyword "char"
    , keyword "boolean"
    , identifier
    ]

subroutineDecNode :: (MonadState [Token] m, MonadError String m) => m AstNode
subroutineDecNode =
  BranchNode SubroutineDec
    <$> sequence
      [ oneOf $ keyword <$> ["constructor", "function", "method"]
      , oneOf [keyword "void", typeNode]
      , identifier
      , symbol '('
      , parameterListNode
      , symbol ')'
      , subroutineBodyNode
      ]

parameterListNode :: (MonadState [Token] m, MonadError String m) => m AstNode
parameterListNode =
  BranchNode ParameterList
    <$> oneOf [p, pure []]
 where
  p =
    concatM
      [ sequence
          [ typeNode
          , identifier
          ]
      , many (sequence [symbol ',', typeNode, identifier]) <&> concat
      ]

subroutineBodyNode :: (MonadState [Token] m, MonadError String m) => m AstNode
subroutineBodyNode =
  BranchNode SubroutineBody
    <$> concatM
      [ symbol '{' <&> singleton
      , many varDecNode
      , statementsNode <&> singleton
      , symbol '}' <&> singleton
      ]

varDecNode :: (MonadState [Token] m, MonadError String m) => m AstNode
varDecNode =
  BranchNode VarDec
    <$> concatM
      [ sequence
          [ keyword "var"
          , typeNode
          , identifier
          ]
      , many (sequence [symbol ',', identifier]) <&> concat
      , symbol ';' <&> singleton
      ]

statementsNode :: (MonadState [Token] m, MonadError String m) => m AstNode
statementsNode =
  BranchNode Statements
    <$> many statementNode

statementNode :: (MonadState [Token] m, MonadError String m) => m AstNode
statementNode =
  oneOf
    [ letStatementNode
    , ifStatementNode
    , whileStatementNode
    , doStatementNode
    , returnStatementNode
    ]

letStatementNode :: (MonadState [Token] m, MonadError String m) => m AstNode
letStatementNode =
  BranchNode LetStatement
    <$> concatM
      [ sequence
          [ keyword "let"
          , identifier
          ]
      , optional
          ( sequence
              [ symbol '['
              , expressionNode
              , symbol ']'
              ]
          )
          <&> fromMaybe []
      , sequence
          [ symbol '='
          , expressionNode
          , symbol ';'
          ]
      ]

ifStatementNode :: (MonadState [Token] m, MonadError String m) => m AstNode
ifStatementNode =
  BranchNode IfStatement
    <$> concatM
      [ sequence
          [ keyword "if"
          , symbol '('
          , expressionNode
          , symbol ')'
          , symbol '{'
          , statementsNode
          , symbol '}'
          ]
      , optional
          ( sequence
              [ keyword "else"
              , symbol '{'
              , statementsNode
              , symbol '}'
              ]
          )
          <&> fromMaybe []
      ]

whileStatementNode :: (MonadState [Token] m, MonadError String m) => m AstNode
whileStatementNode =
  BranchNode WhileStatement
    <$> sequence
      [ keyword "while"
      , symbol '('
      , expressionNode
      , symbol ')'
      , symbol '{'
      , statementsNode
      , symbol '}'
      ]

doStatementNode :: (MonadState [Token] m, MonadError String m) => m AstNode
doStatementNode =
  BranchNode DoStatement
    <$> concatM
      [ keyword "do" <&> singleton
      , subroutineCallNode
      , symbol ';' <&> singleton
      ]

returnStatementNode :: (MonadState [Token] m, MonadError String m) => m AstNode
returnStatementNode =
  BranchNode ReturnStatement
    <$> concatM
      [ keyword "return" <&> singleton
      , optional expressionNode <&> maybeToList
      , symbol ';' <&> singleton
      ]

expressionNode :: (MonadState [Token] m, MonadError String m) => m AstNode
expressionNode =
  BranchNode Expression
    <$> concatM
      [ termNode <&> singleton
      , many (sequence [opNode, termNode]) <&> concat
      ]

termNode :: (MonadState [Token] m, MonadError String m) => m AstNode
termNode =
  BranchNode Term
    <$> oneOf
      [ singleton <$> integerConstant
      , singleton <$> stringConstant
      , singleton <$> keywordConstantNode
      , sequence [symbol '(', expressionNode, symbol ')']
      , sequence [unaryOpNode, termNode]
      , subroutineCallNode
      , validate
          >> sequence [identifier, symbol '[', expressionNode, symbol ']']
      , identifier <&> singleton
      ]
 where
  validate =
    peekHeadOrThrow >>= \curr ->
      assertIsIdentifier curr
        >> peekNextOrThrow
        >>= assertEqual (Symbol '[')

subroutineCallNode :: (MonadState [Token] m, MonadError String m) => m [AstNode]
subroutineCallNode =
  oneOf
    [ validate1 >> p1
    , validate2 >> p2
    ]
 where
  validate1 =
    peekHeadOrThrow >>= \curr ->
      assertIsIdentifier curr
        >> peekNextOrThrow
        >>= assertEqual (Symbol '(')
  p1 =
    sequence
      [ identifier
      , symbol '('
      , expressionListNode
      , symbol ')'
      ]
  validate2 =
    peekHeadOrThrow >>= \curr ->
      assertIsIdentifier curr
        >> peekNextOrThrow
        >>= assertEqual (Symbol '.')
  p2 =
    sequence
      [ identifier
      , symbol '.'
      , identifier
      , symbol '('
      , expressionListNode
      , symbol ')'
      ]

expressionListNode :: (MonadState [Token] m, MonadError String m) => m AstNode
expressionListNode =
  BranchNode ExpressionList
    <$> oneOf [p, pure []]
 where
  p =
    concatM
      [ expressionNode <&> singleton
      , many (sequence [symbol ',', expressionNode]) <&> concat
      ]

opNode :: (MonadState [Token] m, MonadError String m) => m AstNode
opNode =
  oneOf $
    symbol <$> ['+', '-', '*', '/', '&', '|', '<', '>', '=']

unaryOpNode :: (MonadState [Token] m, MonadError String m) => m AstNode
unaryOpNode =
  oneOf $
    symbol <$> ['-', '~']

keywordConstantNode :: (MonadState [Token] m, MonadError String m) => m AstNode
keywordConstantNode =
  oneOf $
    keyword <$> ["true", "false", "null", "this"]
