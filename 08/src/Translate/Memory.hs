module Translate.Memory (translate) where

import Control.Monad.Except

import Translate.Memory.Pop qualified as Pop
import Translate.Memory.Push qualified as Push
import Types

translate :: (MonadError String m) => FilePath -> MemoryCommand -> m [AsmCommand]
translate filePath (Pop segment i) = Pop.translate filePath segment i
translate filePath (Push segment i) = Push.translate filePath segment i
