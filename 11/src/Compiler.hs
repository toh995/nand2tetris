module Compiler where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Functor

import AST
import SymbolTable (SymbolTable)
import SymbolTable qualified as S
import VmCmd

concatMapM :: (Monad m, Traversable t) => (a -> m [b]) -> t a -> m [b]
concatMapM f = fmap concat . mapM f

concatM :: (Monad m) => [m [a]] -> m [a]
concatM ms = sequence ms <&> concat

data ReadObj = ReadObj
  { symbolTable :: SymbolTable
  , className :: ClassName
  }

type ClassName = String

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
  let symbolTable = foldl (flip S.insertV) S.empty c.classVars
      symbolTable' = foldl (flip S.insertS) symbolTable c.subroutineDecs
   in evalStateT
        ( concatMapM
            (subroutineDecToVmCmds c.name symbolTable')
            c.subroutineDecs
        )
        initialLabelIdx

subroutineDecToVmCmds :: (MonadState LabelIndex m, MonadError String m) => ClassName -> SymbolTable -> SubroutineDec -> m [VmCmd]
subroutineDecToVmCmds className symbolTable s@SubroutineDec{kind = ConstructorKind} =
  let symbolTable' = foldl (flip S.insertV) symbolTable (s.args ++ s.localVars)
      readObj = ReadObj{className, symbolTable = symbolTable'}
   in runReaderT
        ( do
            let functionName = className ++ "." ++ s.name
            let numFieldVars =
                  length
                    . filter (\var -> FieldVar == var.kind)
                    . S.variables
                    $ symbolTable
            let headCmds =
                  [ Function FunctionDef{functionName, numVars = length s.localVars}
                  , Memory $ Push Constant numFieldVars
                  , Function FunctionCall{functionName = "Memory.alloc", numArgs = 1}
                  , Memory $ Pop Pointer 0
                  ]
            tailCmds <- concatMapM toVmCmds s.statements
            pure $ headCmds ++ tailCmds
        )
        readObj
subroutineDecToVmCmds className symbolTable s@SubroutineDec{kind = MethodKind} =
  let this = Variable{name = "this", kind = ArgVar, type' = className, index = 0}
      symbolTable' = foldl (flip S.insertV) symbolTable ([this] ++ s.args ++ s.localVars)
      readObj = ReadObj{className, symbolTable = symbolTable'}
   in runReaderT
        ( concatM
            [ pure [Function FunctionDef{functionName = className ++ "." ++ s.name, numVars = length s.localVars}]
            , toVmCmds (VarTerm this.name)
            , pure [Memory $ Pop Pointer 0]
            , concatMapM toVmCmds s.statements
            ]
        )
        readObj
subroutineDecToVmCmds className symbolTable s@SubroutineDec{kind = FunctionKind} =
  let symbolTable' = foldl (flip S.insertV) symbolTable (s.args ++ s.localVars)
      readObj = ReadObj{className, symbolTable = symbolTable'}
   in runReaderT
        ( do
            let functionName = className ++ "." ++ s.name
            concatM
              [ pure [Function FunctionDef{functionName, numVars = length s.localVars}]
              , concatMapM toVmCmds s.statements
              ]
        )
        readObj

class ToVmCmds a where
  toVmCmds :: (MonadError String m, MonadReader ReadObj m, MonadState LabelIndex m) => a -> m [VmCmd]

instance ToVmCmds Statement where
  toVmCmds (LetStatement varName expr) = do
    headCmds <- toVmCmds expr
    r <- ask
    var <- S.lookupV varName r.symbolTable
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
  toVmCmds s@SimpleSubroutineCall{} = do
    r <- ask
    let functionName = r.className ++ "." ++ s.name
    subroutineDec <- S.lookupS s.name r.symbolTable
    if subroutineDec.kind == MethodKind
      then
        concatM
          [ toVmCmds ThisKeyword
          , concatMapM toVmCmds s.exprs
          , pure [Function $ FunctionCall{functionName, numArgs = 1 + length s.exprs}]
          ]
      else
        concatM
          [ concatMapM toVmCmds s.exprs
          , pure [Function $ FunctionCall{functionName, numArgs = length s.exprs}]
          ]
  toVmCmds s@CompoundSubroutineCall{} =
    ask >>= \r ->
      tryError (S.lookupV s.leftName r.symbolTable)
        >>= \case
          Right var ->
            concatM
              [ toVmCmds (VarTerm var.name)
              , concatMapM toVmCmds s.exprs
              , pure [Function $ FunctionCall{functionName = var.type' ++ "." ++ s.rightName, numArgs = 1 + length s.exprs}]
              ]
          Left _ ->
            concatM
              [ concatMapM toVmCmds s.exprs
              , pure [Function $ FunctionCall{functionName = s.leftName ++ "." ++ s.rightName, numArgs = length s.exprs}]
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
  toVmCmds ThisKeyword = pure [Memory $ Push Pointer 0]
  toVmCmds (VarTerm varName) = do
    r <- ask
    var <- S.lookupV varName r.symbolTable
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
