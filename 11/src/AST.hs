module AST where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Functor

import SymbolTable (SymbolTable, VarName, Variable, index, segment)
import SymbolTable qualified as S
import VmCmd

data CompilerState = CompilerState
  { symbolTable :: SymbolTable
  , labelIdx :: Int
  }

data Class = Class
  { name :: String
  , classVars :: [Variable]
  , subroutineDecs :: [SubroutineDec]
  }
  deriving (Show)

data SubroutineDec = SubroutineDec
  { name :: String
  , parentClassName :: String
  , args :: [Variable]
  , localVars :: [Variable]
  , statements :: [Statement]
  }
  deriving (Show)

data Statement
  = LetStatement VarName Expr
  | IfStatement
      { expr :: Expr
      , ifStatements :: [Statement]
      , elseStatements :: [Statement]
      }
  | WhileStatement
  | DoStatement SubroutineCall
  | ReturnStatement (Maybe Expr)
  deriving (Show)

data SubroutineCall = SubroutineCall
  { name :: String
  , exprs :: [Expr]
  }
  deriving (Show)

data Expr
  = SingleExpr Term
  | BinaryExpr Term BinaryOp Expr
  deriving (Show)

data Term
  = IntLiteralTerm Int
  | TrueLiteral
  | FalseLiteral
  | NullLiteral
  | VarTerm VarName
  | ExprTerm Expr
  | UnaryOpTerm UnaryOp Term
  deriving (Show)

-- \| StringLiteral String
-- \| ThisKeyword
-- \| ArrayAccessTerm
-- \| SubroutineCall
-- \| ExpressionTerm

data UnaryOp = AstNeg | AstNot
  deriving (Show)

data BinaryOp
  = AstAdd
  | -- | Sub
    AstMul
  deriving (Show)

-- \| Div
-- \| And
-- \| Or
-- \| Lt
-- \| Gt
-- \| Eq

concatMapM :: (Monad m, Traversable t) => (a -> m [b]) -> t a -> m [b]
concatMapM f = fmap concat . mapM f

concatM :: (Monad m) => [m [a]] -> m [a]
concatM ms = sequence ms <&> concat

classToVmCmds :: (MonadError String m) => Class -> m [VmCmd]
classToVmCmds c =
  runReaderT
    (concatMapM toVmCmds c.subroutineDecs)
    symbolTable
 where
  symbolTable = foldr S.insert S.empty c.classVars

subroutineDecToVmCmds :: (MonadError String m) => SymbolTable -> SubroutineDec -> m [VmCmd]
subroutineDecToVmCmds symbolTable s =
  runReaderT m symbolTable'
 where
  m = do
    let functionName = s.parentClassName ++ "." ++ s.name
    let headCmd = Function FunctionDef{functionName, numVars = length s.localVars}
    tailCmds <- concatMapM toVmCmds s.statements
    pure $ headCmd : tailCmds
  symbolTable' =
    foldr
      S.insert
      symbolTable
      (s.args ++ s.localVars)

-- class ToVmCmds a where
--   toVmCmds :: (MonadReader SymbolTable m) => a -> m [VmCmd]
class ToVmCmds a where
  toVmCmds :: (MonadError String m, MonadReader SymbolTable m) => a -> m [VmCmd]

-- instance ToVmCmds Class where
--   toVmCmds c =
--     local
--       (const symbolTable)
--       (concatMapM toVmCmds c.subroutineDecs)
--    where
--     symbolTable = foldr S.insert S.empty c.classVars

instance ToVmCmds SubroutineDec where
  toVmCmds s = do
    let functionName = s.parentClassName ++ "." ++ s.name
    let headCmd = Function FunctionDef{functionName, numVars = length s.localVars}
    tailCmds <- concatMapM toVmCmds s.statements
    pure $ headCmd : tailCmds

instance ToVmCmds Statement where
  toVmCmds (LetStatement varName expr) = do
    headCmds <- toVmCmds expr
    var <- ask >>= S.lookup varName
    let tailCmd = Memory $ Pop (segment var) (index var)
    pure $ headCmds ++ [tailCmd]
  toVmCmds (i@IfStatement{}) = pure []
  toVmCmds (DoStatement subroutineCall) = toVmCmds subroutineCall
  toVmCmds (ReturnStatement (Just expr)) =
    concatM
      [ toVmCmds expr
      , pure [Function Return]
      ]
  toVmCmds (ReturnStatement Nothing) =
    pure
      [ Memory $ Push Constant 0
      , Function Return
      ]

instance ToVmCmds SubroutineCall where
  toVmCmds s =
    concatM
      [ concatMapM toVmCmds s.exprs
      , pure [Function $ FunctionCall{functionName = s.name, numArgs = length s.exprs}]
      ]

instance ToVmCmds Expr where
  toVmCmds (SingleExpr t) = toVmCmds t
  toVmCmds (BinaryExpr t1 op expr) =
    concatM
      [ toVmCmds t1
      , toVmCmds expr
      , toVmCmds op
      ]

instance ToVmCmds Term where
  toVmCmds (IntLiteralTerm n) = pure [Memory $ Push Constant n]
  toVmCmds TrueLiteral = pure [Memory $ Push Constant 1, Arithmetic Neg]
  toVmCmds FalseLiteral = pure [Memory $ Push Constant 0]
  toVmCmds NullLiteral = pure [Memory $ Push Constant 0]
  toVmCmds (VarTerm varName) = do
    var <- ask >>= S.lookup varName
    pure [Memory $ Push (segment var) (index var)]
  toVmCmds (ExprTerm expr) = toVmCmds expr
  toVmCmds (UnaryOpTerm op expr) =
    concatM
      [ toVmCmds expr
      , toVmCmds op
      ]

instance ToVmCmds UnaryOp where
  toVmCmds AstNeg = pure [Arithmetic Neg]
  toVmCmds AstNot = pure [Logical Not]

instance ToVmCmds BinaryOp where
  toVmCmds AstAdd = pure [Arithmetic Add]
  toVmCmds AstMul = pure [Function $ FunctionCall "Math.multiply" 2]
