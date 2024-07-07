module Main where

import Control.Monad.Except
import Control.Monad.IO.Class
import Data.List
import System.Directory
import System.Environment
import System.FilePath

import AST
import Parse
import VmCmd

jackExt :: String
jackExt = ".jack"

vmExt :: String
vmExt = ".vm"

main :: IO ()
main =
  runExceptT main' >>= \case
    Left err -> error err
    _ -> pure ()

main' :: (MonadIO m, MonadError String m) => m ()
main' =
  liftIO getArgs >>= \case
    [path] ->
      getJackPaths path
        >>= mapM_ writeVmFile
    args ->
      throwError $
        "Expected exactly 1 arg, got " ++ (show . length $ args)

getJackPaths :: (MonadIO m, MonadError String m) => FilePath -> m [FilePath]
getJackPaths path
  | hasExtension path = assertJackExt path >> pure [path]
  | otherwise = do
      let dirPath = dropTrailingPathSeparator path
      filePaths <- liftIO (listDirectory dirPath)
      pure
        ( map (dirPath </>)
            . filter hasJackExt
            $ filePaths
        )

writeVmFile :: (MonadIO m, MonadError String m) => FilePath -> m ()
writeVmFile filePath = do
  assertJackExt filePath
  fileContents <- liftIO $ readFile filePath
  astClass <- parseClass fileContents
  vmCmds <- classToVmCmds astClass
  let fileContents' =
        intercalate "\n"
          . map toStr
          $ vmCmds
  let filePath' = filePath -<.> vmExt
  liftIO $ writeFile filePath' fileContents'

assertJackExt :: (MonadError String m) => FilePath -> m ()
assertJackExt path
  | hasJackExt path = pure ()
  | otherwise =
      throwError $
        "Expected file extension '" ++ jackExt ++ "', got extension '" ++ takeExtension path ++ "' in filepath '" ++ path ++ "'"

hasJackExt :: FilePath -> Bool
hasJackExt = (== jackExt) . takeExtension
