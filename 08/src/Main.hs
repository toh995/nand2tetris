module Main where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Functor
import Data.List
import System.Directory
import System.Environment
import System.FilePath

import Parse
import Translate
import Translate.Function qualified as Function
import Types

main :: IO ()
main =
  runExceptT main' >>= \case
    Left err -> error err
    _ -> pure ()

main' :: (MonadIO m, MonadError String m) => m ()
main' =
  liftIO getArgs >>= \case
    [path] ->
      if hasExtension path
        then fileArg path
        else dirArg path
    args ->
      throwError $
        "Expected exactly 1 arg, got " ++ (show . length $ args)

fileArg :: (MonadIO m, MonadError String m) => FilePath -> m ()
fileArg filePath = do
  vmCommands <- readVmCommands filePath
  asmCommands <- evalStateT (translate filePath vmCommands) initialCount
  let fileContents' = intercalate "\n" asmCommands
  let filePath' = replaceExtension filePath ".asm"
  liftIO $ writeFile filePath' fileContents'

dirArg :: (MonadIO m, MonadError String m) => FilePath -> m ()
dirArg d = do
  let dirPath = dropTrailingPathSeparator d
  filePaths <-
    liftIO (listDirectory dirPath)
      <&> map (dirPath </>)
        . filter
          ((== ".vm") . takeExtension)
  vmCommands <- mapM readVmCommands filePaths
  asmCommands <-
    evalStateT
      ( do
          baseAsmCmds <- zipWithM translate filePaths vmCommands
          sysInit <- Function.translate (FunctionCall "Sys.init" 0)
          pure $ asmPrefix ++ sysInit ++ concat baseAsmCmds
      )
      initialCount
  let fileContents' = intercalate "\n" asmCommands
  let filePath' = dirPath </> takeBaseName dirPath <.> ".asm"
  liftIO $ writeFile filePath' fileContents'

readVmCommands :: (MonadIO m, MonadError String m) => FilePath -> m [VmCommand]
readVmCommands filePath
  | takeExtension filePath == ".vm" =
      liftIO (readFile filePath)
        >>= parseVmCommands
  | otherwise =
      throwError $
        "Expected file extension '.vm', got extension '" ++ takeExtension filePath ++ "' in filepath '" ++ filePath ++ "'"

asmPrefix :: [AsmCommand]
asmPrefix =
  [ "@256"
  , "D=A"
  , "@SP"
  , "M=D"
  ]
