module Translate where

import Control.Monad.Except
import Control.Monad.State

import Translate.Arithmetic qualified as Arithmetic
import Translate.Logical (initialCount)
import Translate.Logical qualified as Logical
import Translate.Memory qualified as Memory
import Types

translate :: (MonadError String m) => FilePath -> [VmCommand] -> m [[AsmCommand]]
translate filePath vmCmds =
  evalStateT
    (mapM (translate' filePath) vmCmds)
    initialCount

translate' ::
  (MonadError String m, MonadState Counter m) =>
  FilePath ->
  VmCommand ->
  m [AsmCommand]
translate' _ (Arithmetic cmd) = pure . Arithmetic.translate $ cmd
translate' _ (Logical cmd) = Logical.translate cmd
translate' filePath (Memory cmd) = Memory.translate filePath cmd
