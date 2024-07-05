{-# LANGUAGE TemplateHaskell #-}

module SymbolTable where

import Control.Lens hiding (index)
import Data.HashMap.Lazy qualified as HashMap

import AST
import Control.Monad.State

data SymbolTable = SymbolTable
  { _hm :: HashMap.HashMap String Variable
  , _fieldVarIdx :: Int
  , _staticVarIdx :: Int
  , _localVarIdx :: Int
  , _argVarIdx :: Int
  }
makeLenses ''SymbolTable

empty :: SymbolTable
empty =
  SymbolTable
    { _hm = HashMap.empty
    , _fieldVarIdx = 0
    , _staticVarIdx = 0
    , _localVarIdx = 0
    , _argVarIdx = 0
    }

insert :: Variable -> SymbolTable -> SymbolTable
insert v = execState stateMonad
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
    modify $ hm . at v.name ?~ v'
    modify $ case v.kind of
      FieldVar -> fieldVarIdx +~ 1
      StaticVar -> staticVarIdx +~ 1
      LocalVar -> localVarIdx +~ 1
      ArgVar -> argVarIdx +~ 1

-- \| StaticVar | LocalVar | ArgVar

-- let s' = s{hm = HashMap.insert v.name v s.hm}
--  in s'

-- buildSymbol :: Variable -> SymbolTable -> Variable
-- buildSymbol v st = case v.kind of
--   FieldVar -> Symbol{name = v.name, kind = v.kind, type' = v.varType, index = st.fieldVarIdx}
