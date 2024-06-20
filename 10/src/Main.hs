module Main where

import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Functor
import System.Directory
import System.Environment
import System.FilePath

import AST.Build
import AST.ToXML
import Token.Parse

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
  | hasExtension path = assertJackExt path >> pure [path]
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
  assertJackExt filePath
  fileContents <- liftIO $ readFile filePath
  fileContents' <- buildTokensXmlStr fileContents
  let filePath' = dropExtension filePath ++ "T" ++ xmlExt
  liftIO $ writeFile filePath' fileContents'

doClassXml :: (MonadIO m, MonadError String m) => FilePath -> m ()
doClassXml filePath = do
  assertJackExt filePath
  fileContents <- liftIO $ readFile filePath
  fileContents' <- buildClassXmlStr fileContents
  let filePath' = replaceExtension filePath xmlExt
  liftIO $ writeFile filePath' fileContents'

buildTokensXmlStr :: (MonadError String m) => String -> m String
buildTokensXmlStr sourceCode =
  parseTokenList sourceCode
    >>= buildTokensAST
    <&> show . astToXml

buildClassXmlStr :: (MonadError String m) => String -> m String
buildClassXmlStr sourceCode =
  parseTokenList sourceCode
    >>= buildClassAST
    <&> show . astToXml

assertJackExt :: (MonadError String m) => FilePath -> m ()
assertJackExt path
  | hasJackExt path = pure ()
  | otherwise =
      throwError $
        "Expected file extension '" ++ jackExt ++ "', got extension '" ++ takeExtension path ++ "' in filepath '" ++ path ++ "'"

hasJackExt :: FilePath -> Bool
hasJackExt = (== jackExt) . takeExtension
