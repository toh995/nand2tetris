module AST where

import Token

data BranchNodeType
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
  = LeafNode Token
  | BranchNode BranchNodeType [AstNode]
