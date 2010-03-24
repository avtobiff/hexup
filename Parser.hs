module Parser where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Types


xqueryupdatedef = LanguageDef
	{ commentStart   = "(:",
	  commentEnd     = ":)",
	  commentLine    = "",
	  nestedComments = False,
	  identStart     = letter <|> char '_',
	  identLetter    = letter <|> digit <|> oneOf ".-_",
	  -- here CombiningChar and Extender have to be added
	  -- see: http://www.w3.org/TR/REC-xml/#NT-CombiningChar
	  opStart        = opLetter emptyDef,
	  opLetter       = oneOf ":!#$",
	  reservedOpNames = [],
	  reservedNames    = [],
	  caseSensitive   = True
	}

lexer :: P.TokenParser ()
lexer  = P.makeTokenParser 
         (xqueryupdatedef
	 )	 

whiteSpace= P.whiteSpace lexer
lexeme    = P.lexeme lexer
symbol    = P.symbol lexer
natural   = P.natural lexer
parens    = P.parens lexer
semi      = P.semi lexer
identifier= P.identifier lexer
reserved  = P.reserved lexer
reservedOp= P.reservedOp lexer


run p input = 
    case (parse p "" input) of
        Left err -> do putStr "parse error at "
                       print err
        Right x -> print x


-- function to run a parser, in case of a parse error 
-- the message is printed out to the screen (by function error)
parseIt :: Parser a -> String -> a
parseIt p i = case parse p "" i of
                  Right x -> x
                  Left x -> error ("parseIt: Error in input\n" ++ (show x))

-- returns a list of Expressions, possibly has to be changed
-- because the comma-character can also occur within Expressions!
exprParser :: Parser [ExprSingle]
exprParser = do { res <- sepBy1 (try flworExpr <|> deleteExpr <|> insertExpr <|> try replaceExpr <|> renameExpr) $ char ','
		   ; return res
}




flworExpr :: Parser ExprSingle
flworExpr = do
    fl <- many $ forClause <|> letClause
    w <- many whereClause
    whiteSpace
    o <- many orderbyClause
    whiteSpace
    r <- returnClause
    return (FLWORExpr (fl++w++o++[r]))


-- function returns a list of symbols occuring in some clause
parseForSymbols :: Parser [String]
parseForSymbols = do { 
	r <- many parseForSymbol;
	return r}


-- function returns a symbol occuring in some clause
parseForSymbol :: Parser String
parseForSymbol = try $ do manyTill anyChar $ try $ char '$'
                          r <- identifier
                          return r


parseTes = many par

par = do r <- try $ char 's'
         return r

-- the source and target in an insert Expression have always to be
-- sorrounded by round brackets

insertExpr :: Parser ExprSingle
insertExpr = do { string "insert"
				; whiteSpace
				; string "node"
				; option []  (string "s")
				; whiteSpace
				; src <- parens expr
				; whiteSpace
				; targChoice <- insTargetChoice
				; whiteSpace
				; targ <- parens expr
				; return (InsertExpr src targChoice targ)
				}

insTargetChoice :: Parser InsertExprTargetChoice
insTargetChoice =  try afterParser 
				<|> beforeParser 
				<|> try asFirstParser 
				<|> try asLastParser
					where 
						afterParser = do {string "after"; return After}
						beforeParser = do {string "before" ; return Before}
						asFirstParser = do {string "as first into" ; return AsFirst}
						asLastParser = do { option "" (string "as last")
										  ; whiteSpace
										  ; string "into"
										  ; return AsLast }


-- target in a delete expression has to be surrounded by brackets
-- this is exploited by the parens parser
deleteExpr :: Parser ExprSingle
deleteExpr = do { string "delete"
				; whiteSpace
				; string "node"
				; option [] (string "s")
				; whiteSpace
				; targ <- parens expr
				; return $ DeleteExpr targ
				}

-- target and source have to be in brackets in replaceExpressions
replaceExpr :: Parser ExprSingle
replaceExpr = do { string "replace"
				 ; whiteSpace
				 ; valOf <- option [] (string "value of")
				 ; whiteSpace
				 ; string "node"
				 ; whiteSpace
				 ; targ <- parens expr
				 ; whiteSpace
				 ; string "with"
				 ; whiteSpace
				 ; src <- parens expr
				 ; return $ ReplaceExpr (valToBool valOf) targ src
				}
				where 	
					valToBool []  = False
					valToBool "value of"   = True

renameExpr :: Parser ExprSingle
renameExpr = do { string "rename node"
				; whiteSpace
				; targ <- parens expr
				; whiteSpace
				; string "as"
				; whiteSpace
				; newName <- identifier
				; return $ RenameExpr targ newName
}



forClause :: Parser FLWORExpr
forClause = do
    string "for"
    whiteSpace
    char '$'
    id <- identifier
    whiteSpace
    -- td <- typeDeclaration
    -- whiteSpace
    string "in"
    whiteSpace
    char '('
    xp <- expressions
    char ')'
    whiteSpace
    return (ForClause id xp)


letClause :: Parser FLWORExpr
letClause = do
    string "let"
    whiteSpace
    char '$'
    id <- identifier
    whiteSpace
    string ":="
    whiteSpace
    char '('
    xp <- expressions
    char ')'
    whiteSpace
    return (LetClause id xp)

expr :: Parser String
expr = many1 $ noneOf "(,)"

comma :: Parser ()
comma = skipMany1 (space <|> char ',')

expressions :: Parser [String]
expressions = do ids <- sepBy1 expr comma
                 return ids


whereClause :: Parser FLWORExpr
whereClause = do
    string "where"
    whiteSpace
    expr <- manyTill anyChar $ try $ oneOf " ,\n"
    return (WhereClause expr) 


-- TODO
orderbyClause :: Parser FLWORExpr
orderbyClause = do {
    ; string "order"
    ; whiteSpace
    ; string "by"
    ; whiteSpace
	; char '$'
	; id <- identifier
	; xp <- option [] (do {char '/'; res <- sepBy1 (identifier <|> atPlusIdentifier) (char '/'); return res} )
	; return (OrderByClause id xp)
}
	where
		atPlusIdentifier = do {	at <- string "@"
							  ; id <- identifier
							  ; return (at ++ id)
		}
{-
    osl <- do OrderSpec
              many $ char ","
              many OrderSpec
 -}


returnClause :: Parser FLWORExpr
returnClause = do
    string "return"
    whiteSpace
    res <- manyTill anyChar eof
    return (ReturnClause res)


