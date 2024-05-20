module Main3 where

import Control.Monad.State
import Data.Functor
import Data.HashMap.Lazy qualified as HashMap
import Data.List
import System.Environment

type Line = String
type LabelName = String
type VarName = String

main :: IO ()
main = do
  filePath <- getArgs <&> head
  fileContents <- readFile filePath
  let ls =
        filter (/= "")
          . map (removeComments . removeWhitespace)
          . lines
          $ fileContents
  pure ()

removeWhitespace :: Line -> Line
removeWhitespace = filter (/= ' ')

removeComments :: Line -> Line
removeComments [] = []
removeComments ('/' : '/' : _) = []
removeComments (x : xs) = x : removeComments xs

buildLabelMap :: [Line] -> HashMap.HashMap LabelName Int
buildLabelMap lines' =
  evalState
    (mapM helperState lines')
    initialS
 where
  initialS = (0, HashMap.empty)
  helperState :: Line -> State (Int, HashMap.HashMap) HashMap.HashMap
  helperState line = do
