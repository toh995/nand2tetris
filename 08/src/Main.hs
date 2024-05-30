module Main where

import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Functor
import System.Environment
import System.FilePath

import Parse
import Translate

type Arg = String

main :: IO ()
main =
  runExceptT main' >>= \case
    Left err -> error err
    _ -> pure ()

main' :: (MonadIO m, MonadError String m) => m ()
main' = do
  filePath <- liftIO getArgs >>= extractFilePath
  fileContents <- liftIO $ readFile filePath
  commands <- parseCommands fileContents
  fileContents' <- translate filePath commands <&> unlines . concat
  let filePath' = replaceExtension filePath ".asm"
  liftIO $ writeFile filePath' fileContents'

extractFilePath :: (MonadError String m) => [Arg] -> m FilePath
extractFilePath [filePath]
  | takeExtension filePath == ".vm" = pure filePath
  | otherwise =
      throwError $
        "Expected file extension '.vm', got extension '" ++ takeExtension filePath ++ "' in filepath '" ++ filePath ++ "'"
extractFilePath args =
  throwError $
    "Expected exactly 1 arg, got " ++ (show . length $ args)
