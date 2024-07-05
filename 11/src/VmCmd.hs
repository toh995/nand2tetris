module VmCmd where

type LabelName = String

data VmCmd
  = Arithmetic ArithmeticCommand
  | Branching BranchingCommand
  | Function FunctionCommand
  | Logical LogicalCommand
  | Memory MemoryCommand
  deriving (Show)

data ArithmeticCommand
  = Add
  | Sub
  | Neg
  deriving (Show)

data BranchingCommand
  = Label LabelName
  | Goto LabelName
  | IfGoto LabelName
  deriving (Show)

data FunctionCommand
  = FunctionCall
      { functionName :: String
      , numArgs :: Int
      }
  | FunctionDef
      { functionName :: String
      , numVars :: Int
      }
  | Return
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

class ToStr a where
  toStr :: a -> String

instance ToStr VmCmd where
  toStr :: VmCmd -> String
  toStr (Arithmetic Add) = "add"
  toStr (Arithmetic Sub) = "sub"
  toStr (Arithmetic Neg) = "neg"
  toStr (Branching (Label labelName)) = "label " ++ labelName
  toStr (Branching (Goto labelName)) = "goto " ++ labelName
  toStr (Branching (IfGoto labelName)) = "if-goto " ++ labelName
  toStr (Function (FunctionCall{functionName, numArgs})) = "call " ++ functionName ++ " " ++ show numArgs
  toStr (Function (FunctionDef{functionName, numVars})) = "function " ++ functionName ++ " " ++ show numVars
  toStr (Function Return) = "return"
  toStr (Logical Eq) = "eq"
  toStr (Logical Gt) = "gt"
  toStr (Logical Lt) = "lt"
  toStr (Logical And) = "and"
  toStr (Logical Or) = "or"
  toStr (Logical Not) = "not"
  toStr (Memory (Push segment n)) = "push " ++ toStr segment ++ " " ++ show n
  toStr (Memory (Pop segment n)) = "pop " ++ toStr segment ++ " " ++ show n

instance ToStr Segment where
  toStr Local = "local"
  toStr Argument = "argument"
  toStr This = "this"
  toStr That = "that"
  toStr Constant = "constant"
  toStr Static = "static"
  toStr Temp = "temp"
  toStr Pointer = "pointer"
