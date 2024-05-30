module Translate where

import Control.Monad.Except
import Control.Monad.State
import Data.Functor

import Translate.Arithmetic qualified as Arithmetic
import Translate.Branching qualified as Branching
import Translate.Function qualified as Function
import Translate.Logical qualified as Logical
import Translate.Memory qualified as Memory
import Types

initialCount :: Counter
initialCount = 0

translate :: (MonadError String m, MonadState Counter m) => FilePath -> [VmCommand] -> m [AsmCommand]
translate filePath vmCommands =
  mapM
    (translate' filePath)
    vmCommands
    <&> concat

translate' ::
  (MonadError String m, MonadState Counter m) =>
  FilePath ->
  VmCommand ->
  m [AsmCommand]
translate' _ (Arithmetic cmd) = pure . Arithmetic.translate $ cmd
translate' _ (Branching cmd) = pure . Branching.translate $ cmd
translate' _ (Function cmd) = Function.translate cmd
translate' _ (Logical cmd) = Logical.translate cmd
translate' filePath (Memory cmd) = Memory.translate filePath cmd
