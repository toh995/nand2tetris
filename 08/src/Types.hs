module Types where

type Counter = Int
type LabelName = String

type AsmCommand = String

data VmCommand
  = Arithmetic ArithmeticCommand
  | Logical LogicalCommand
  | Memory MemoryCommand
  deriving (Show)

data ArithmeticCommand
  = Add
  | Sub
  | Neg
  deriving (Show)

data LogicalCommand
  = Eq
  | Gt
  | Lt
  | And
  | Or
  | Not
  deriving (Show)

data MemoryCommand
  = Push Segment Int
  | Pop Segment Int
  deriving (Show)

data Segment
  = Local
  | Argument
  | This
  | That
  | Constant
  | Static
  | Temp
  | Pointer
  deriving (Show)
