module Main where

import Data.Functor
import Data.List
import System.Environment
import System.FilePath

import Parse
import Translate

main :: IO ()
main = do
  filePath <- getArgs <&> head
  fileContents <- readFile filePath
  let fileContents' =
        intercalate "\n"
          . translate
          . parseLines
          $ fileContents
  let filePath' = replaceExtension filePath ".hack"
  writeFile filePath' fileContents'
