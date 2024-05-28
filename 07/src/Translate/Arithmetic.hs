module Translate.Arithmetic (translate) where

import Types

translate :: ArithmeticCommand -> [AsmCommand]
translate Add =
  [ "// add"
  , "@SP"
  , "M=M-1"
  , "A=M"
  , "D=M"
  , "@SP"
  , "A=M-1"
  , "M=M+D"
  ]
translate Sub =
  [ "// sub"
  , "@SP"
  , "M=M-1"
  , "A=M"
  , "D=M"
  , "@SP"
  , "A=M-1"
  , "M=M-D"
  ]
translate Neg =
  [ "// neg"
  , "@SP"
  , "A=M-1"
  , "M=-M"
  ]
