module Types where

import XML

-- data Token
--   = Keyword KeywordString
--   | Symbol SymbolChar
--   | IntegerConstant Int
--   | StringConstant String
--   | Identifier String
--
-- newtype KeywordString = KeywordString String
--
legalKeywords :: [String]
legalKeywords = ["class", "constructor", "function", "method", "field", "static", "var", "int", "char", "boolean", "void", "true", "false", "null", "this", "let", "do", "if", "else", "while", "return"]

legalSymbols :: [Char]
legalSymbols = ['{', '}', '(', ')', '[', ']', '.', ',', ';', '+', '-', '*', '/', '&', '|', '<', '>', '=', '~']

--
-- newtype SymbolChar = SymbolChar Char
--
-- data Class
--   = Class
--       (Keyword "class")
--       ClassName
--       (Symbol '{')
--       [ClassVarDec]
--       [SubroutineDec]
--       (Symbol '}')

-- data Token
-- data LeafKind
--   = Keyword String
--   | Symbol Char
--   | IntegerConstant Int
--   | StringConstant String
--   | Identifier String
--
-- data InnerKind
--   = Class

data AstNode
  = Keyword String
  | Symbol Char
  | IntegerConstant Int
  | StringConstant String
  | Identifier String
  | Tokens [AstNode]
  | Class [AstNode]
  | ClassVarDec [AstNode]
  | SubroutineDec [AstNode]
  | ParameterList [AstNode]
  | SubroutineBody [AstNode]
  | VarDec [AstNode]
  | Statements [AstNode]
  | LetStatement [AstNode]
  | IfStatement [AstNode]
  | WhileStatement [AstNode]
  | DoStatement [AstNode]
  | ReturnStatement [AstNode]
  | Expression [AstNode]
  | Term [AstNode]
  | ExpressionList [AstNode]
  deriving (Show)

toXml :: AstNode -> XmlTreeNode
toXml (Keyword s) = XmlLeafNode{tagLabel = "keyword", body = s}
toXml (Symbol c) = XmlLeafNode{tagLabel = "symbol", body = [c]}
toXml (IntegerConstant i) = XmlLeafNode{tagLabel = "integerConstant", body = show i}
toXml (StringConstant s) = XmlLeafNode{tagLabel = "stringConstant", body = s}
toXml (Identifier s) = XmlLeafNode{tagLabel = "identifier", body = s}
toXml (Tokens ns) = XmlBranchNode{tagLabel = "tokens", children = map toXml ns}
toXml (Class ns) = XmlBranchNode{tagLabel = "class", children = map toXml ns}
toXml (ClassVarDec ns) = XmlBranchNode{tagLabel = "classVarDec", children = map toXml ns}
toXml (SubroutineDec ns) = XmlBranchNode{tagLabel = "subroutineDec", children = map toXml ns}
toXml (ParameterList ns) = XmlBranchNode{tagLabel = "parameterList", children = map toXml ns}
toXml (SubroutineBody ns) = XmlBranchNode{tagLabel = "subroutineBody", children = map toXml ns}
toXml (VarDec ns) = XmlBranchNode{tagLabel = "varDec", children = map toXml ns}
toXml (Statements ns) = XmlBranchNode{tagLabel = "statements", children = map toXml ns}
toXml (LetStatement ns) = XmlBranchNode{tagLabel = "letStatement", children = map toXml ns}
toXml (IfStatement ns) = XmlBranchNode{tagLabel = "ifStatement", children = map toXml ns}
toXml (WhileStatement ns) = XmlBranchNode{tagLabel = "whileStatement", children = map toXml ns}
toXml (DoStatement ns) = XmlBranchNode{tagLabel = "doStatement", children = map toXml ns}
toXml (ReturnStatement ns) = XmlBranchNode{tagLabel = "returnStatement", children = map toXml ns}
toXml (Expression ns) = XmlBranchNode{tagLabel = "expression", children = map toXml ns}
toXml (Term ns) = XmlBranchNode{tagLabel = "term", children = map toXml ns}
toXml (ExpressionList ns) = XmlBranchNode{tagLabel = "expressionList", children = map toXml ns}

-- toXml Symbol Char
-- toXml IntegerConstant Int
-- toXml StringConstant String
-- toXml Identifier String
-- toXml Tokens [AstNode]
-- toXml Class [AstNode]
-- toXml ClassVarDec [AstNode]
-- toXml SubroutineDec [AstNode]
-- toXml ParameterList [AstNode]
-- toXml SubroutineBody [AstNode]
-- toXml VarDec [AstNode]
-- toXml Statements [AstNode]
-- toXml LetStatement [AstNode]
-- toXml IfStatement [AstNode]
-- toXml WhileStatement [AstNode]
-- toXml DoStatement [AstNode]
-- toXml ReturnStatement [AstNode]
-- toXml Expression [AstNode]

-- data LeafNode
--   = Keyword String
--   | Symbol Char
--   | IntegerConstant Int
--   | StringConstant String
--   | Identifier String
--
-- data InnerNode
--   = Tokens
--   | Class
--   | ClassVarDec
--   | SubroutineDec
--   | ParameterList
--   | SubroutineBody
--   | VarDec
--   | Statements
--   | LetStatement
--   | IfStatement
--   | WhileStatement
--   | DoStatement
--   | ReturnStatement
--   | Expression
--

-- type XmlString = String
--
-- toXmlString :: TreeNode -> XmlString
-- toXmlString (Keyword s) = "<keyword>" ++ s ++ "</keyword>"
-- toXmlString (Symbol s) = "<symbol>" ++ show s ++ "</symbol>"
-- toXmlString (IntegerConstant s) = "<integerConstant>" ++ show s ++ "</integerConstant>"
-- toXmlString (StringConstant s) = "<stringConstant>" ++ s ++ "</stringConstant>"
-- toXmlString (Identifier s) = "<identifier>" ++ s ++ "</identifier>"
