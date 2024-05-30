module Translate.Memory.Push where

import Control.Monad.Except
import System.FilePath

import Types

pushDtoStack :: [AsmCommand]
pushDtoStack =
  [ "@SP"
  , "M=M+1"
  , "A=M-1"
  , "M=D"
  ]

translate :: (MonadError String m) => FilePath -> Segment -> Int -> m [AsmCommand]
translate _ Local i =
  pure $
    [ "// push local " ++ show i
    , "@" ++ show i
    , "D=A"
    , "@LCL"
    , "A=D+M"
    , "D=M"
    ]
      ++ pushDtoStack
translate _ Argument i =
  pure $
    [ "// push argument " ++ show i
    , "@" ++ show i
    , "D=A"
    , "@ARG"
    , "A=D+M"
    , "D=M"
    ]
      ++ pushDtoStack
translate _ This i =
  pure $
    [ "// push this " ++ show i
    , "@" ++ show i
    , "D=A"
    , "@THIS"
    , "A=D+M"
    , "D=M"
    ]
      ++ pushDtoStack
translate _ That i =
  pure $
    [ "// push that " ++ show i
    , "@" ++ show i
    , "D=A"
    , "@THAT"
    , "A=D+M"
    , "D=M"
    ]
      ++ pushDtoStack
translate _ Constant i =
  pure $
    [ "// push constant " ++ show i
    , "@" ++ show i
    , "D=A"
    ]
      ++ pushDtoStack
translate filePath Static i =
  pure $
    [ "// push static " ++ show i
    , "@" ++ takeBaseName filePath ++ "." ++ show i
    , "D=M"
    ]
      ++ pushDtoStack
translate _ Temp i =
  pure $
    [ "// push temp " ++ show i
    , "@" ++ show (i + 5)
    , "D=M"
    ]
      ++ pushDtoStack
translate _ Pointer 0 =
  pure $
    [ "// push pointer 0"
    , "@THIS"
    , "D=M"
    ]
      ++ pushDtoStack
translate _ Pointer 1 =
  pure $
    [ "// push pointer 1"
    , "@THAT"
    , "D=M"
    ]
      ++ pushDtoStack
translate _ segment i =
  throwError $
    "Invalid 'push' operation, for segment '" ++ show segment ++ "' and value '" ++ show i ++ "'"
