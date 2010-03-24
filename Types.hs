module Types where


import qualified Data.Map as M
import Text.XML.HXT.DOM
import Text.XML.HXT.Parser
import Text.XML.HXT.XPath


-- Internal types, program/code related.

-- Key is a string (identifier), value is a string (XML is not typed).
-- The second list contains the name of all identifiers and an f it
-- was a for clause and an l if it was a let clause
type Env = (M.Map Identifier XmlTree, [(Identifier, Char)])
type File = String -- For now, probably som XML later.

type Tuple = (Identifier,Value)
type TupleStream = [Tuple]


-- Grammar types

type Value = XmlTrees -- For now, what later? Do we need different types?!

type XPathExpr = String
type Identifier = String
type ReturnExpr = String

data ExprSingle = FLWORExpr [FLWORExpr]
				| InsertExpr SourceExpr InsertExprTargetChoice TargetExpr
				| DeleteExpr TargetExpr
				| RenameExpr TargetExpr NewNameExpr
				| ReplaceExpr ValueReplace TargetExpr XPathExpr
				| OrExpr
                deriving Show

data InsertExprTargetChoice = AsFirst | AsLast | Before | After
	deriving Show

type ValueReplace = Bool
type SourceExpr = XPathExpr
type TargetExpr = XPathExpr
type NewNameExpr = String


data FLWORExpr = ForClause Identifier [XPathExpr]
               | LetClause Identifier [XPathExpr]
               | WhereClause XPathExpr --ExprSingle
               | OrderByClause Identifier [String] --an orderBy looks e.g. like this: order by $x/prices
               | ReturnClause XPathExpr
               deriving Show

-- Placeholder hack to get through the type checker
type OrderModifier = String
data OrderSpec = OrderSpec ExprSingle OrderModifier
               deriving Show
