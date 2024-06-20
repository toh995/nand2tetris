module AST.ToXML (astToXml) where

import AST
import Token.ToXML
import XML

astToXml :: AstNode -> XmlTreeNode
astToXml (LeafNode token) = tokenToXml token
astToXml (BranchNode nodeType childNodes) =
  XmlBranchNode
    { tagLabel = xmlTagLabelFor nodeType
    , children = map astToXml childNodes
    }

xmlTagLabelFor :: BranchNodeType -> String
xmlTagLabelFor Tokens = "tokens"
xmlTagLabelFor Class = "class"
xmlTagLabelFor ClassVarDec = "classVarDec"
xmlTagLabelFor SubroutineDec = "subroutineDec"
xmlTagLabelFor ParameterList = "parameterList"
xmlTagLabelFor SubroutineBody = "subroutineBody"
xmlTagLabelFor VarDec = "varDec"
xmlTagLabelFor Statements = "statements"
xmlTagLabelFor LetStatement = "letStatement"
xmlTagLabelFor IfStatement = "ifStatement"
xmlTagLabelFor WhileStatement = "whileStatement"
xmlTagLabelFor DoStatement = "doStatement"
xmlTagLabelFor ReturnStatement = "returnStatement"
xmlTagLabelFor Expression = "expression"
xmlTagLabelFor Term = "term"
xmlTagLabelFor ExpressionList = "expressionList"
