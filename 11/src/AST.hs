module AST where

import SymbolTable (VarName, Variable)

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
      { expr :: Expr
      , statements :: [Statement]
      }
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
  | SubroutineCallTerm SubroutineCall
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
  | AstSub
  | AstMul
  | AstDiv
  | AstAnd
  | AstOr
  | AstLt
  | AstGt
  | AstEq
  deriving (Show)
