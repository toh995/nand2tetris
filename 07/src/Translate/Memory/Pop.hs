module Translate.Memory.Pop where

import Control.Monad.Except
import System.FilePath

import Types

popToD :: [AsmCommand]
popToD =
  [ "@SP"
  , "M=M-1"
  , "A=M"
  , "D=M"
  ]

translate :: (MonadError String m) => FilePath -> Segment -> Int -> m [AsmCommand]
translate _ Local i =
  pure $
    [ "// pop local " ++ show i
    , "@" ++ show i
    , "D=A"
    , "@LCL"
    , "D=D+M"
    , "@R13"
    , "M=D"
    ]
      ++ popToD
      ++ [ "@R13"
         , "A=M"
         , "M=D"
         ]
translate _ Argument i =
  pure $
    [ "// pop argument " ++ show i
    , "@" ++ show i
    , "D=A"
    , "@ARG"
    , "D=D+M"
    , "@R13"
    , "M=D"
    ]
      ++ popToD
      ++ [ "@R13"
         , "A=M"
         , "M=D"
         ]
translate _ This i =
  pure $
    [ "// pop this " ++ show i
    , "@" ++ show i
    , "D=A"
    , "@THIS"
    , "D=D+M"
    , "@R13"
    , "M=D"
    ]
      ++ popToD
      ++ [ "@R13"
         , "A=M"
         , "M=D"
         ]
translate _ That i =
  pure $
    [ "// pop that " ++ show i
    , "@" ++ show i
    , "D=A"
    , "@THAT"
    , "D=D+M"
    , "@R13"
    , "M=D"
    ]
      ++ popToD
      ++ [ "@R13"
         , "A=M"
         , "M=D"
         ]
translate _ Constant _ =
  throwError "Invalid 'pop' operation, for segment 'Constant'"
translate filePath Static i =
  pure $
    ["// pop static " ++ show i]
      ++ popToD
      ++ [ "@" ++ takeBaseName filePath ++ "." ++ show i
         , "M=D"
         ]
translate _ Temp i =
  pure $
    ["// pop temp " ++ show i]
      ++ popToD
      ++ [ "@" ++ show (i + 5)
         , "M=D"
         ]
translate _ Pointer 0 =
  pure $
    ["// pop pointer 0"]
      ++ popToD
      ++ [ "@THIS"
         , "M=D"
         ]
translate _ Pointer 1 =
  pure $
    ["// pop pointer 1"]
      ++ popToD
      ++ [ "@THAT"
         , "M=D"
         ]
translate _ segment i =
  throwError $
    "Invalid 'pop' operation, for segment '" ++ show segment ++ "' and value '" ++ show i ++ "'"
