module Translate.Branching where

import Translate.Memory.Pop (popToD)
import Types

translate :: BranchingCommand -> [AsmCommand]
translate (Label labelName) =
  [ "// label " ++ labelName
  , "(" ++ labelName ++ ")"
  ]
translate (Goto labelName) =
  [ "// goto " ++ labelName
  , "@" ++ labelName
  , "0;JMP"
  ]
translate (IfGoto labelName) =
  ["// if-goto " ++ labelName]
    ++ popToD
    ++ [ "@" ++ labelName
       , "D;JNE"
       ]
