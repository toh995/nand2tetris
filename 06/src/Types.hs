module Types where

data Line
  = LabelLine String
  | AInstructionLine String
  | CInstructionLine
      { dest :: BinaryString
      , comp :: BinaryString
      , jump :: BinaryString
      }
  deriving (Show)

type BinaryString = String
type BinaryChar = Char
