module AST where

import VmCmd

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

data Variable = Variable
  { name :: String
  , kind :: VarKind
  , type' :: String
  , index :: Int
  }
  deriving (Show)

data VarKind = FieldVar | StaticVar | LocalVar | ArgVar
  deriving (Show)

data Statement
  = LetStatement Variable Expr
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
  | ExprTerm Expr
  | UnaryOpTerm UnaryOp Term
  deriving (Show)

-- \| StringLiteral String
-- \| TrueLiteral
-- \| FalseLiteral
-- \| NullLiteral
-- \| ThisKeyword
-- \| VarTerm String
-- \| ArrayAccessTerm
-- \| SubroutineCall
-- \| ExpressionTerm

data UnaryOp = AstNeg | AstNot
  deriving (Show)

data BinaryOp
  = AstAdd
  | -- | Sub
    AstMult
  deriving (Show)

-- \| Div
-- \| And
-- \| Or
-- \| Lt
-- \| Gt
-- \| Eq

instance ToVmCmds Class where
  toVmCmds Class{subroutineDecs} = concatMap toVmCmds subroutineDecs

instance ToVmCmds SubroutineDec where
  toVmCmds s =
    let functionName = s.parentClassName ++ "." ++ s.name
        headCmd = Function FunctionDef{functionName, numVars = length s.localVars}
        tailCmds = concatMap toVmCmds s.statements
     in headCmd : tailCmds

instance ToVmCmds Statement where
  toVmCmds (DoStatement subroutineCall) = toVmCmds subroutineCall
  toVmCmds (ReturnStatement (Just expr)) = toVmCmds expr ++ [Function Return]
  toVmCmds (ReturnStatement Nothing) = [Memory $ Push Constant 0, Function Return]

-- toVmCmds (LetStatement var expr) = toVmCmds expr ++ [Memory (Pop)]

instance ToVmCmds SubroutineCall where
  toVmCmds s =
    concatMap toVmCmds s.exprs
      ++ [ Function $ FunctionCall{functionName = s.name, numArgs = length s.exprs}
         ]

instance ToVmCmds Expr where
  toVmCmds (SingleExpr t) = toVmCmds t
  toVmCmds (BinaryExpr t1 op expr) =
    concat
      [ toVmCmds t1
      , toVmCmds expr
      , toVmCmds op
      ]

instance ToVmCmds Term where
  toVmCmds (IntLiteralTerm n) = [Memory $ Push Constant n]
  toVmCmds (ExprTerm expr) = toVmCmds expr
  toVmCmds (UnaryOpTerm op expr) = toVmCmds expr ++ toVmCmds op

instance ToVmCmds UnaryOp where
  toVmCmds AstNeg = [Arithmetic Neg]
  toVmCmds AstNot = [Logical Not]

instance ToVmCmds BinaryOp where
  toVmCmds AstAdd = [Arithmetic Add]
  toVmCmds AstMult = [Function $ FunctionCall "Math.multiply" 2]
