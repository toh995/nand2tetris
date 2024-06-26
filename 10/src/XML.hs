module XML where

import Data.List

data XmlTreeNode
  = XmlLeafNode
      { tagLabel :: String
      , body :: String
      }
  | XmlBranchNode
      { tagLabel :: String
      , children :: [XmlTreeNode]
      }

instance Show XmlTreeNode where
  show XmlLeafNode{tagLabel, body} =
    "<" ++ tagLabel ++ "> " ++ body' ++ " </" ++ tagLabel ++ ">"
   where
    body' = case body of
      "<" -> "&lt;"
      ">" -> "&gt;"
      "\"" -> "&quot;"
      "&" -> "&amp;"
      _ -> body
  show XmlBranchNode{tagLabel, children} =
    "<" ++ tagLabel ++ ">\n" ++ body ++ "</" ++ tagLabel ++ ">"
   where
    body =
      unlines
        . map ("  " ++)
        . lines
        . intercalate "\n"
        . map show
        $ children
