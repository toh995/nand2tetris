module AST where

import VmCmd

data Class = Class
  { name :: String
  , classVars :: [Variable]
  , subroutineDecs :: [SubroutineDec]
  }
  deriving (Show)

type VarName = String

data Variable = Variable
  { name :: VarName
  , kind :: VarKind
  , type' :: String
  , index :: Int
  }
  deriving (Show)

data VarKind = FieldVar | StaticVar | LocalVar | ArgVar
  deriving (Eq, Show)

segment :: Variable -> Segment
segment Variable{kind} =
  case kind of
    FieldVar -> This
    StaticVar -> Static
    LocalVar -> Local
    ArgVar -> Argument

data SubroutineDec = SubroutineDec
  { name :: String
  , args :: [Variable]
  , localVars :: [Variable]
  , statements :: [Statement]
  , kind :: SubroutineKind
  }
  deriving (Show)

data SubroutineKind = ConstructorKind | FunctionKind | MethodKind
  deriving (Eq, Show)

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

data SubroutineCall
  = SimpleSubroutineCall
      { name :: String
      , exprs :: [Expr]
      }
  | CompoundSubroutineCall
      { leftName, rightName :: String
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
  | ThisKeyword
  | VarTerm VarName
  | SubroutineCallTerm SubroutineCall
  | ExprTerm Expr
  | UnaryOpTerm UnaryOp Term
  deriving (Show)

-- \| StringLiteral String
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
