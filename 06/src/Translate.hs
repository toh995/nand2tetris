module Translate (translate) where

import Control.Applicative
import Control.Monad.State
import Data.HashMap.Lazy qualified as HashMap
import Data.Tuple.HT
import GHC.Utils.Monad
import Text.Read hiding (get)

import Types

type LabelMap = HashMap.HashMap LabelName Int
type VarMap = HashMap.HashMap VarName Int
type LabelName = String
type VarName = String
type Counter = Int

translate :: [Line] -> [BinaryString]
translate ls =
  buildBinaryStrings
    (buildLabelMap ls)
    ls

buildLabelMap :: [Line] -> LabelMap
buildLabelMap ls =
  snd $ execState (helperS ls) initialS
 where
  initialS = (0, HashMap.empty)
  helperS :: [Line] -> State (Counter, LabelMap) ()
  helperS [] = pure ()
  helperS (LabelLine label : ls') =
    modify
      ( \(counter, labelMap) ->
          (counter, HashMap.insert label counter labelMap)
      )
      >> helperS ls'
  helperS (_ : ls') =
    modify (mapFst (+ 1))
      >> helperS ls'

buildBinaryStrings :: LabelMap -> [Line] -> [BinaryString]
buildBinaryStrings labelMap ls =
  evalState
    (mapMaybeM (binaryStringS labelMap) ls)
    initialS
 where
  initialS = (16, HashMap.empty)

binaryStringS :: LabelMap -> Line -> State (Counter, VarMap) (Maybe BinaryString)
binaryStringS _ (LabelLine _) = pure Nothing
binaryStringS _ (CInstructionLine{dest, comp, jump}) =
  pure . Just $ "111" ++ comp ++ dest ++ jump
binaryStringS labelMap (AInstructionLine text) = do
  int <- aTextToIntS labelMap text
  let binaryStr = intToBinary15 int
  pure $ Just ('0' : binaryStr)

aTextToIntS :: LabelMap -> String -> State (Counter, VarMap) Int
aTextToIntS labelMap text = do
  (counter, varMap) <- get
  let maybeInt =
        readMaybe text
          <|> builtinSymbolToInt text
          <|> HashMap.lookup text labelMap
          <|> HashMap.lookup text varMap
  case maybeInt of
    (Just n) -> pure n
    _ ->
      counter
        <$ put
          (counter + 1, HashMap.insert text counter varMap)

intToBinary15 :: Int -> BinaryString
intToBinary15 n =
  evalState
    (mapM computeBinChar [14, 13 .. 0])
    (n `mod` (2 ^ (15 :: Int)))
 where
  computeBinChar :: Int -> State Int BinaryChar
  computeBinChar k =
    get >>= \n' ->
      if n' >= (2 ^ k)
        then put (n' - (2 ^ k)) >> pure '1'
        else pure '0'

builtinSymbolToInt :: String -> Maybe Int
builtinSymbolToInt "R0" = Just 0
builtinSymbolToInt "R1" = Just 1
builtinSymbolToInt "R2" = Just 2
builtinSymbolToInt "R3" = Just 3
builtinSymbolToInt "R4" = Just 4
builtinSymbolToInt "R5" = Just 5
builtinSymbolToInt "R6" = Just 6
builtinSymbolToInt "R7" = Just 7
builtinSymbolToInt "R8" = Just 8
builtinSymbolToInt "R9" = Just 9
builtinSymbolToInt "R10" = Just 10
builtinSymbolToInt "R11" = Just 11
builtinSymbolToInt "R12" = Just 12
builtinSymbolToInt "R13" = Just 13
builtinSymbolToInt "R14" = Just 14
builtinSymbolToInt "R15" = Just 15
builtinSymbolToInt "SCREEN" = Just 16384
builtinSymbolToInt "KBD" = Just 24576
builtinSymbolToInt "SP" = Just 0
builtinSymbolToInt "LCL" = Just 1
builtinSymbolToInt "ARG" = Just 2
builtinSymbolToInt "THIS" = Just 3
builtinSymbolToInt "THAT" = Just 4
builtinSymbolToInt _ = Nothing
