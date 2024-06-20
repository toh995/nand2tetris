module Token.ToXML (tokenToXml) where

import Token
import XML

tokenToXml :: Token -> XmlTreeNode
tokenToXml token =
  XmlLeafNode
    { tagLabel = xmlTagLabelFor token
    , body = xmlBodyFor token
    }

xmlTagLabelFor :: Token -> String
xmlTagLabelFor (Keyword _) = "keyword"
xmlTagLabelFor (Symbol _) = "symbol"
xmlTagLabelFor (IntegerConstant _) = "integerConstant"
xmlTagLabelFor (StringConstant _) = "stringConstant"
xmlTagLabelFor (Identifier _) = "identifier"

xmlBodyFor :: Token -> String
xmlBodyFor (Keyword s) = s
xmlBodyFor (Symbol c) = [c]
xmlBodyFor (IntegerConstant i) = show i
xmlBodyFor (StringConstant s) = s
xmlBodyFor (Identifier s) = s
