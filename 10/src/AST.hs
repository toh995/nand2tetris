module AST where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Either
import Data.Functor

-- import Pipes
--

-- data AstNode
--   = AstLeaf AstToken
--   | AstBranch AstBranchType [AstNode]
--
data Token
  = Keyword String
  | Symbol Char
  | IntegerConstant Int
  | StringConstant String
  | Identifier String
  deriving (Eq, Show)

--
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

--
-- -- :: Consumer AstToken m Node
-- -- = do
-- --

-- class (Monad m) => MonadIterator m a where
--   next :: m (Maybe a)
--
-- newtype IteratorT m a = IteratorT {runIteratorT :: ()}

-- class Iterator i a where
--   next :: i -> a

-- newtype ConstIterator a = ConstIterator {a :: a}
-- instance Iterator (ConstIterator a) a where
--   next ConstIterator{a} = a

-- newtype ListIterator a = ListIterator {as :: [a]}
-- instance Iterator (ListIterator a) a where
--   next (ListIterator (a:as)) = a

-- newtype ListIterator a = ListIterator {runListIterator :: [a]}
--
-- instance Functor ListIterator where
--   fmap g (ListIterator as) = ListIterator $ map g as
--
-- instance Applicative ListIterator where
--   pure a = ListIterator [a]
--
-- instance Monad (ListIterator m) where
--   ListIterator as >>= g =
class MonadListIter l m where
  peek :: m (Maybe l)
  next :: m (Maybe l)

peekOrThrow :: (MonadListIter l m, MonadError String m) => m l
peekOrThrow =
  peek >>= \case
    Just l -> pure l
    Nothing -> throwError "asdfasdf"

nextOrThrow :: (MonadListIter l m, MonadError String m) => m l
nextOrThrow =
  peek >>= \case
    Just l -> pure l
    Nothing -> throwError "asdfasdf"

newtype ListIterT l m a = ListIterT {unListIterT :: StateT [l] m a}

instance (Monad m) => MonadListIter l (ListIterT l m) where
  peek =
    ListIterT $
      gets
        ( \case
            [] -> Nothing
            (l : _) -> Just l
        )
  next =
    ListIterT
      ( modify
          ( \case
              (_ : ls) -> ls
              [] -> []
          )
      )
      >> peek

instance (Functor m) => Functor (ListIterT l m) where
  fmap f (ListIterT s) = ListIterT $ fmap f s

instance (Monad m) => Applicative (ListIterT l m) where
  pure a = ListIterT $ pure a
  ListIterT s1 <*> ListIterT s2 = ListIterT $ s1 <*> s2

instance (Monad m) => Monad (ListIterT l m) where
  (ListIterT s) >>= f =
    ListIterT $
      s >>= unListIterT . f

--
-- instance (MonadError String m) => MonadListIterator l (ListIteratorT l m) where
--   next =
--     ListIteratorT $
--       get >>= \case
--         [] -> throwError "Cannot read from empty list!"
--         (l : ls) -> put ls $> l
--
-- evalListIteratorT :: (Monad m) => ListIteratorT l m a -> [l] -> m a
-- evalListIteratorT (ListIteratorT s) = evalStateT s

-- validate :: (Eq a, Show a, MonadError String m) => a -> a -> m ()
-- validate expected actual
--   | expected == actual = pure ()
--   | otherwise =
--       throwError $
--         "Expected token '" ++ show expected ++ "', instead got '" ++ show actual ++ "'"
--
-- nextValidate :: (MonadListIterator Token m, MonadError String m) => Token -> m Token
-- nextValidate expected =
--   next >>= \case
--     Nothing -> throwError " asdfasdfs"
--     Just x ->
--       if x /= expected
--         then throwError "asdfasdf"
--         else pure x
--
-- keyword :: (MonadListIterator Token m, MonadError String m) => String -> m AstNode
-- keyword s = nextValidate (Keyword s) $> TokenNode (Keyword s)

-- tokenNode :: (MonadListIterator Token m, MonadError String m) => Token -> m AstNode
-- tokenNode token =
--   next >>= \case
--     Nothing -> throwError "asdfasdfs"
--     Just x ->
--       if x /= token
--         then throwError "asdfasdf"
--         else pure . TokenNode $ token

keyword :: (MonadListIter Token m, MonadError String m) => String -> m AstNode
keyword s =
  peekOrThrow
    >>= ( \token ->
            if token == Keyword s
              then pure $ TokenNode (Keyword s)
              else
                throwError "asdfasd"
        )

symbol :: (MonadListIter Token m, MonadError String m) => Char -> m AstNode
symbol c =
  peekOrThrow >>= \token ->
    if token == Symbol c
      then pure $ TokenNode (Symbol c)
      else throwError "asdfasd"

integerConstant :: (MonadListIter Token m, MonadError String m) => m AstNode
integerConstant =
  peekOrThrow >>= \case
    IntegerConstant x -> pure $ TokenNode (IntegerConstant x)
    _ -> throwError "asdfasdf"

stringConstant :: (MonadListIter Token m, MonadError String m) => m AstNode
stringConstant =
  peekOrThrow >>= \case
    StringConstant x -> pure $ TokenNode (StringConstant x)
    _ -> throwError "asdfasdf"

identifier :: (MonadListIter Token m, MonadError String m) => m AstNode
identifier =
  peekOrThrow >>= \case
    Identifier x -> pure $ TokenNode (Identifier x)
    _ -> throwError "asdfasdf"

classNode :: (MonadListIter Token m, MonadError String m, Alternative m) => m AstNode
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
      -- nodes3 <- many soubroutineDec
      node4 <- symbol '}'
      pure $ nodes1 ++ nodes2

classVarDecNode :: (MonadListIter Token m, MonadError String m, Alternative m) => m AstNode
classVarDecNode =
  CompositeNode ClassVarDec
    <$> sequence
      [ keyword "static" <|> keyword "field"
      , typeNode
      , identifier
      ]

typeNode :: (MonadListIter Token m, MonadError String m, Alternative m) => m AstNode
typeNode =
  keyword "int"
    <|> keyword "char"
    <|> keyword "boolean"
    <|> identifier

-- keyword :: (MonadListIterator Token m, MonadError String m) => String -> m AstNode
-- keyword s = do
--   token <- next >>= liftEither
--   validate (Keyword s) token
--   pure $ TokenNode (Keyword s)
--
-- symbol ::
--   ( MonadListIterator String m
--   , MonadError String m
--   ) =>
--   Char ->
--   m AstNode
-- symbol c = do
--   token <- next >>= liftEither
--   if token == Symbol c
--     then pure (TokenNode (Symbol c))
--     else
--       throwError $
--         "Expected token '" ++ show (Symbol c) ++ "', instead got '" ++ show token ++ "'"

-- keyword :: (MonadListIterator AstToken m, MonadError String m) => String -> m Node
-- keyword s = do
--   token <- next
--   if token == Keyword s
--     then pure Node
--     else throwError "asdfsadf"
