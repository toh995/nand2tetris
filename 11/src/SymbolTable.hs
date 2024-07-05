{-# LANGUAGE TemplateHaskell #-}

module SymbolTable where

import Control.Lens hiding (index)
import Control.Monad.Except
import Control.Monad.State
import Data.HashMap.Lazy qualified as HashMap

import AST

data SymbolTable = SymbolTable
  { _vHM :: HashMap.HashMap String Variable
  , _sHM :: HashMap.HashMap String SubroutineDec
  , _fieldVarIdx :: Int
  , _staticVarIdx :: Int
  , _localVarIdx :: Int
  , _argVarIdx :: Int
  }
makeLenses ''SymbolTable

empty :: SymbolTable
empty =
  SymbolTable
    { _vHM = HashMap.empty
    , _sHM = HashMap.empty
    , _fieldVarIdx = 0
    , _staticVarIdx = 0
    , _localVarIdx = 0
    , _argVarIdx = 0
    }

insertV :: Variable -> SymbolTable -> SymbolTable
insertV v = execState stateMonad
 where
  stateMonad :: (MonadState SymbolTable m) => m ()
  stateMonad = do
    symbolTable <- get
    let nextIdx = case v.kind of
          FieldVar -> symbolTable ^. fieldVarIdx
          StaticVar -> symbolTable ^. staticVarIdx
          LocalVar -> symbolTable ^. localVarIdx
          ArgVar -> symbolTable ^. argVarIdx
    let v' = v{index = nextIdx}
    modify $ vHM . at v.name ?~ v'
    modify $ case v.kind of
      FieldVar -> fieldVarIdx +~ 1
      StaticVar -> staticVarIdx +~ 1
      LocalVar -> localVarIdx +~ 1
      ArgVar -> argVarIdx +~ 1

lookupV :: (MonadError String m) => VarName -> SymbolTable -> m Variable
lookupV varName symbolTable =
  case symbolTable ^. vHM . at varName of
    (Just v) -> pure v
    Nothing -> throwError $ "Could not find Variable '" ++ varName ++ "' in SymbolTable"

variables :: SymbolTable -> [Variable]
variables = HashMap.elems . _vHM

insertS :: SubroutineDec -> SymbolTable -> SymbolTable
insertS s symbolTable = symbolTable & sHM . at s.name ?~ s

lookupS :: (MonadError String m) => String -> SymbolTable -> m SubroutineDec
lookupS name symbolTable =
  case symbolTable ^. sHM . at name of
    (Just s) -> pure s
    Nothing -> throwError $ "Could not find SubroutineDec '" ++ name ++ "' in SymbolTable"
