module Translate.Function where

import Control.Monad.State

import Translate.Memory.Push (pushDtoStack)
import Types

popToDwithR14 :: [AsmCommand]
popToDwithR14 =
  [ "@R14"
  , "M=M-1"
  , "A=M"
  , "D=M"
  ]

translate :: (MonadState Counter m) => FunctionCommand -> m [AsmCommand]
translate (FunctionCall{functionName, numArgs}) = do
  count <- get
  put $ count + 1
  let returnLabel = functionName ++ "_RETURN" ++ show count
  pure $
    ["// call " ++ functionName ++ " " ++ show numArgs]
      ++ ["// Save the current stack frame"]
      ++ ["@" ++ returnLabel, "D=A"]
      ++ pushDtoStack
      ++ ["@LCL", "D=M"]
      ++ pushDtoStack
      ++ ["@ARG", "D=M"]
      ++ pushDtoStack
      ++ ["@THIS", "D=M"]
      ++ pushDtoStack
      ++ ["@THAT", "D=M"]
      ++ pushDtoStack
      ++ [ "// Reset @ARG"
         , "@SP"
         , "D=M"
         , "@" ++ show (numArgs + 5)
         , "D=D-A"
         , "@ARG"
         , "M=D"
         ]
      ++ [ "// Manage the program jumps"
         , "@" ++ functionName
         , "0;JMP"
         , "(" ++ returnLabel ++ ")"
         ]
translate (FunctionDef{functionName, numVars}) =
  pure $
    [ "// function " ++ functionName ++ " " ++ show numVars
    , "(" ++ functionName ++ ")"
    , "@SP"
    , "D=M"
    , "@LCL"
    , "M=D"
    ]
      ++ concat
        (replicate numVars ["A=D", "M=0", "D=D+1"])
      ++ [ "@SP"
         , "M=D"
         ]
translate Return =
  pure $
    [ "// return"
    , "// Store the return address for later usage"
    , "@LCL"
    , "D=M"
    , "@5"
    , "D=D-A"
    , "A=D"
    , "D=M"
    , "@R13"
    , "M=D"
    , "// Copy the return value onto argument 0"
    , "@SP"
    , "A=M-1"
    , "D=M"
    , "@ARG"
    , "A=M"
    , "M=D"
    , "// Reset @SP"
    , "D=A+1"
    , "@SP"
    , "M=D"
    , "//Restore the caller's segment pointers"
    , "@LCL"
    , "D=M"
    , "@R14"
    , "M=D"
    ]
      ++ popToDwithR14
      ++ ["@THAT", "M=D"]
      ++ popToDwithR14
      ++ ["@THIS", "M=D"]
      ++ popToDwithR14
      ++ ["@ARG", "M=D"]
      ++ popToDwithR14
      ++ ["@LCL", "M=D"]
      ++ [ "// Jump to the return address"
         , "@R13"
         , "A=M"
         , "0;JMP"
         ]
