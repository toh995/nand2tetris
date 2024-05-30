module Translate.Logical where

import Control.Monad.State

import Types

translate :: (MonadState Counter m) => LogicalCommand -> m [AsmCommand]
translate Eq = do
  count <- get
  put $ count + 1
  pure
    [ "// eq"
    , "@SP"
    , "M=M-1"
    , "A=M"
    , "D=M"
    , "@SP"
    , "A=M-1"
    , "D=M-D"
    , "@TRUE" ++ show count
    , "D;JEQ"
    , "D=0"
    , "@CONTINUE" ++ show count
    , "0;JMP"
    , "(TRUE" ++ show count ++ ")"
    , "D=-1"
    , "@CONTINUE" ++ show count
    , "0;JMP"
    , "(CONTINUE" ++ show count ++ ")"
    , "@SP"
    , "A=M-1"
    , "M=D"
    ]
translate Gt = do
  count <- get
  put $ count + 1
  pure
    [ "// gt"
    , "@SP"
    , "M=M-1"
    , "A=M"
    , "D=M"
    , "@SP"
    , "A=M-1"
    , "D=M-D"
    , "@TRUE" ++ show count
    , "D;JGT"
    , "D=0"
    , "@CONTINUE" ++ show count
    , "0;JMP"
    , "(TRUE" ++ show count ++ ")"
    , "D=-1"
    , "@CONTINUE" ++ show count
    , "0;JMP"
    , "(CONTINUE" ++ show count ++ ")"
    , "@SP"
    , "A=M-1"
    , "M=D"
    ]
translate Lt = do
  count <- get
  put $ count + 1
  pure
    [ "// lt"
    , "@SP"
    , "M=M-1"
    , "A=M"
    , "D=M"
    , "@SP"
    , "A=M-1"
    , "D=M-D"
    , "@TRUE" ++ show count
    , "D;JLT"
    , "D=0"
    , "@CONTINUE" ++ show count
    , "0;JMP"
    , "(TRUE" ++ show count ++ ")"
    , "D=-1"
    , "@CONTINUE" ++ show count
    , "0;JMP"
    , "(CONTINUE" ++ show count ++ ")"
    , "@SP"
    , "A=M-1"
    , "M=D"
    ]
translate And =
  pure
    [ "// and"
    , "@SP"
    , "M=M-1"
    , "A=M"
    , "D=M"
    , "@SP"
    , "A=M-1"
    , "M=M&D"
    ]
translate Or =
  pure
    [ "// or"
    , "@SP"
    , "M=M-1"
    , "A=M"
    , "D=M"
    , "@SP"
    , "A=M-1"
    , "M=M|D"
    ]
translate Not =
  pure
    [ "// not"
    , "@SP"
    , "A=M-1"
    , "M=!M"
    ]
