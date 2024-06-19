module Main where

import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Functor
import Debug.Trace
import System.Directory
import System.Environment
import System.FilePath

import Parse
import Types

jackExt :: String
jackExt = ".jack"

xmlExt :: String
xmlExt = ".xml"

main :: IO ()
main =
  runExceptT main' >>= \case
    Left err -> error err
    _ -> pure ()

main' :: (MonadIO m, MonadError String m) => m ()
main' =
  liftIO getArgs >>= \case
    [path] ->
      do
        paths <- getJackPaths path
        mapM_ doTokensXml paths
        mapM_ doClassXml paths
    args ->
      throwError $
        "Expected exactly 1 arg, got " ++ (show . length $ args)

getJackPaths :: (MonadIO m, MonadError String m) => FilePath -> m [FilePath]
getJackPaths path
  | hasExtension path = validateJackExt path >> pure [path]
  | otherwise = do
      let dirPath = dropTrailingPathSeparator path
      filePaths <- liftIO (listDirectory dirPath)
      pure
        ( map (dirPath </>)
            . filter hasJackExt
            $ filePaths
        )

doTokensXml :: (MonadIO m, MonadError String m) => FilePath -> m ()
doTokensXml filePath = do
  validateJackExt filePath
  fileContents <- liftIO $ readFile filePath
  fileContents' <- buildTokensXmlStr fileContents
  let filePath' = dropExtension filePath ++ "T" ++ xmlExt
  liftIO $ writeFile filePath' fileContents'

doClassXml :: (MonadIO m, MonadError String m) => FilePath -> m ()
doClassXml filePath = do
  validateJackExt filePath
  fileContents <- liftIO $ readFile filePath
  fileContents' <- buildClassXmlStr fileContents
  let filePath' = replaceExtension filePath xmlExt
  liftIO $ writeFile filePath' fileContents'

buildTokensXmlStr :: (MonadError String m) => String -> m String
buildTokensXmlStr jackSource =
  parseTokens jackSource
    <&> show . toXml

buildClassXmlStr :: (MonadError String m) => String -> m String
buildClassXmlStr jackSource =
  parseClass jackSource
    <&> show . toXml

validateJackExt :: (MonadError String m) => FilePath -> m ()
validateJackExt path
  | hasJackExt path = pure ()
  | otherwise =
      throwError $
        "Expected file extension '" ++ jackExt ++ "', got extension '" ++ takeExtension path ++ "' in filepath '" ++ path ++ "'"

hasJackExt :: FilePath -> Bool
hasJackExt = (== jackExt) . takeExtension
