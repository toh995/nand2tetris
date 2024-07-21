module Compiler where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Functor

import AST
import SymbolTable (SymbolTable, index, segment)
import SymbolTable qualified as S
import VmCmd

concatMapM :: (Monad m, Traversable t) => (a -> m [b]) -> t a -> m [b]
concatMapM f = fmap concat . mapM f

concatM :: (Monad m) => [m [a]] -> m [a]
concatM ms = sequence ms <&> concat

type LabelIndex = Int

initialLabelIdx :: LabelIndex
initialLabelIdx = 0

nextLabel :: (MonadState LabelIndex m) => m LabelName
nextLabel = do
  idx <- get
  put $ idx + 1
  pure $ "L" ++ show idx

classToVmCmds :: (MonadError String m) => Class -> m [VmCmd]
classToVmCmds c =
  let symbolTable = foldl (flip S.insert) S.empty c.classVars
      computation = concatMapM toVmCmds c.subroutineDecs
   in flip runReaderT symbolTable
        . flip evalStateT initialLabelIdx
        $ computation

--  evalStateT
--    ( concatMapM
--        (subroutineDecToVmCmds symbolTable)
--        c.subroutineDecs
--    )
--    initialLabelIdx
-- where
--  symbolTable = foldr S.insert S.empty c.classVars

-- subroutineDecToVmCmds :: (MonadState LabelIndex m, MonadError String m) => SymbolTable -> SubroutineDec -> m [VmCmd]
-- subroutineDecToVmCmds symbolTable s =
--   runReaderT m symbolTable'
--  where
--   m = do
--     let functionName = s.parentClassName ++ "." ++ s.name
--     let headCmd = Function FunctionDef{functionName, numVars = length s.localVars}
--     tailCmds <- concatMapM toVmCmds s.statements
--     pure $ headCmd : tailCmds
--   symbolTable' =
--     foldr
--       S.insert
--       symbolTable
--       (s.args ++ s.localVars)

-- class ToVmCmds a where
--   toVmCmds :: (MonadReader SymbolTable m) => a -> m [VmCmd]
class ToVmCmds a where
  toVmCmds :: (MonadError String m, MonadReader SymbolTable m, MonadState LabelIndex m) => a -> m [VmCmd]

-- instance ToVmCmds Class where
--   toVmCmds c =
--     local
--       (const symbolTable)
--       (concatMapM toVmCmds c.subroutineDecs)
--    where
--     symbolTable = foldr S.insert S.empty c.classVars

instance ToVmCmds SubroutineDec where
  toVmCmds s =
    local
      (\table -> foldl (flip S.insert) table vars)
      ( do
          let functionName = s.parentClassName ++ "." ++ s.name
          let headCmd = Function FunctionDef{functionName, numVars = length s.localVars}
          tailCmds <- concatMapM toVmCmds s.statements
          pure $ headCmd : tailCmds
      )
   where
    vars = s.args ++ s.localVars

-- toVmCmds s = do
--   modify $ \stateObj ->
--     stateObj
--       { symbolTable =
--           foldr
--             S.insert
--             stateObj.symbolTable
--             (s.args ++ s.localVars)
--       }
--   let functionName = s.parentClassName ++ "." ++ s.name
--   let headCmd = Function FunctionDef{functionName, numVars = length s.localVars}
--   tailCmds <- concatMapM toVmCmds s.statements
--   pure $ headCmd : tailCmds

instance ToVmCmds Statement where
  toVmCmds (LetStatement varName expr) = do
    headCmds <- toVmCmds expr
    symbolTable <- ask
    var <- S.lookup varName symbolTable
    let tailCmd = Memory $ Pop (segment var) (index var)
    pure $ headCmds ++ [tailCmd]
  toVmCmds i@IfStatement{} = do
    l1 <- nextLabel
    l2 <- nextLabel
    concatM
      [ toVmCmds i.expr
      , pure [Branching $ IfGoto l1]
      , concatMapM toVmCmds i.elseStatements
      , pure [Branching $ Goto l2]
      , pure [Branching $ Label l1]
      , concatMapM toVmCmds i.ifStatements
      , pure [Branching $ Goto l2]
      , pure [Branching $ Label l2]
      ]
  toVmCmds w@WhileStatement{} = do
    l1 <- nextLabel
    l2 <- nextLabel
    concatM
      [ pure [Branching $ Label l1]
      , toVmCmds w.expr
      , pure [Logical Not]
      , pure [Branching $ IfGoto l2]
      , concatMapM toVmCmds w.statements
      , pure [Branching $ Goto l1]
      , pure [Branching $ Label l2]
      ]
  toVmCmds (DoStatement subroutineCall) =
    concatM
      [ toVmCmds subroutineCall
      , pure [Memory $ Pop Temp 0]
      ]
  toVmCmds (ReturnStatement (Just expr)) =
    concatM
      [ toVmCmds expr
      , pure [Function Return]
      ]
  toVmCmds (ReturnStatement Nothing) =
    pure
      [ Memory $ Push Constant 0
      , Function Return
      ]

instance ToVmCmds SubroutineCall where
  toVmCmds s =
    concatM
      [ concatMapM toVmCmds s.exprs
      , pure [Function $ FunctionCall{functionName = s.name, numArgs = length s.exprs}]
      ]

instance ToVmCmds Expr where
  toVmCmds (SingleExpr t) = toVmCmds t
  toVmCmds (BinaryExpr t1 op expr) =
    concatM
      [ toVmCmds t1
      , toVmCmds expr
      , toVmCmds op
      ]

instance ToVmCmds Term where
  toVmCmds (IntLiteralTerm n) = pure [Memory $ Push Constant n]
  toVmCmds TrueLiteral = pure [Memory $ Push Constant 1, Arithmetic Neg]
  toVmCmds FalseLiteral = pure [Memory $ Push Constant 0]
  toVmCmds NullLiteral = pure [Memory $ Push Constant 0]
  toVmCmds (VarTerm varName) = do
    symbolTable <- ask
    var <- S.lookup varName symbolTable
    pure [Memory $ Push (segment var) (index var)]
  toVmCmds (SubroutineCallTerm s) = toVmCmds s
  toVmCmds (ExprTerm expr) = toVmCmds expr
  toVmCmds (UnaryOpTerm op expr) =
    concatM
      [ toVmCmds expr
      , toVmCmds op
      ]

instance ToVmCmds UnaryOp where
  toVmCmds AstNeg = pure [Arithmetic Neg]
  toVmCmds AstNot = pure [Logical Not]

instance ToVmCmds BinaryOp where
  toVmCmds AstAdd = pure [Arithmetic Add]
  toVmCmds AstSub = pure [Arithmetic Sub]
  toVmCmds AstMul = pure [Function $ FunctionCall "Math.multiply" 2]
  toVmCmds AstDiv = pure [Function $ FunctionCall "Math.divide" 2]
  toVmCmds AstAnd = pure [Logical And]
  toVmCmds AstOr = pure [Logical Or]
  toVmCmds AstLt = pure [Logical Lt]
  toVmCmds AstGt = pure [Logical Gt]
  toVmCmds AstEq = pure [Logical Eq]
