module Consumer where

import Control.Monad.Except
import Pipes

data AstToken
  = Keyword String
  | Symbol Char
  | IntegerConstant Int
  | StringConstant String
  | Identifier String
  deriving (Eq, Show)

data Node = Node

keyword :: (MonadError String m) => String -> Consumer AstToken m Node
keyword s = do
  token <- await
  if token == Keyword s
    then pure Node
    else throwError $ "Expected token '" ++ show (Keyword s)
