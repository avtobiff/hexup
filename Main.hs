module Main where

import qualified Data.Map as M
import Parser
import Types
import qualified Update
import Text.XML.HXT.Parser
import Text.XML.HXT.XPath
import Text.XML.HXT.DOM.TypeDefs
import Text.XML.HXT.DOM.QualifiedName
import qualified Data.List as L
import System( getArgs )
import System.Directory( removeFile )

-- Takes a list of strings (expressions) and a filename and interprets
-- the expressions on the file.
-- Lukas: I added some hard-coded test expressions for convenience
stdpath :: String
stdpath = "examples/test.xml" -- XML File here
--exprs = "let $s := /recipes/recipe/ingredient[@amount=3]\nlet $t := /recipes/recipe/ingredient[@amount=1] return <result1> $s </result1> <result2> $t </result2>" -- Expressions (program) here
stdexprs :: String
stdexprs = "for $s in (/recipes/recipe/ingredient)\nreturn <result>$s</result>\n"
--stdexprs = "for $t in (/orderTest/el)\norder by $t/con/@val return <res>$t</res>\n"


-- Creates an empty Env.
emptyenv :: Env
emptyenv = (M.empty, [])

main :: IO ()
main = do { argv <- getArgs
		  ; validArgs <- checkArgs argv
		  ; srcfile <- readFile (if validArgs then (argv !! 1) else stdpath)
		  ; (srctree, _, _) <- getXmlDocument [(a_validate, "0")] (if validArgs then (argv !! 1) else stdpath)
          -- here the expressions are parsed 
          ; putStrLn "LOADED FILE"
		  -- ; putStrLn $ xshow [srctree]
		  ; exprs <- if validArgs then (return (argv !! 0)) else (return stdexprs)
          ; v <- doexprs srctree emptyenv srcfile (parseIt exprParser exprs)
          ; putStrLn "EXPRESSIONS PARSED"
          ; case v of
              Right (env,f) -> do {
                  targpath <- if validArgs then (return (argv !! 2)) else return "res.xml"
                ; writeFile "restemp.tmp" (removeRoot f)
				; (xmldoc, _, _) <- getXmlDocument [(a_validate, "0")] "restemp.tmp"
				; removeFile "restemp.tmp"
				--; putStrLn $ xshow errs
				; (_, _) <- putXmlDocument [(a_validate, "0"), (a_indent, "1")] targpath xmldoc
				; putStrLn "----- RESULT -----"
				; finalResToPrint <- readFile targpath
				; putStrLn finalResToPrint
			  }
		  -- show environment (for debugging purposes):
		  --; putStrLn $ show (fst env)
		  }
		
removeRoot :: String -> String
removeRoot ('<' : '/' : rest) = take (length (removeUntilEndOfTag rest) - 4) (removeUntilEndOfTag rest)
removeRoot s = s

removeUntilEndOfTag :: String -> String
removeUntilEndOfTag ('>' : rest) = rest
removeUntilEndOfTag (_ : rest) = removeUntilEndOfTag rest

removeRootNode :: XmlTrees -> XmlTrees
removeRootNode ((NTree (XTag QN {namePrefix = "", localPart = "/", namespaceUri = ""} _) succs):_) = succs
removeRootNode someTree = someTree

checkArgs :: [String] -> IO Bool
checkArgs (_:_:_:[]) = return True
checkArgs _ = do { putStrLn "ERROR: Bad number of arguments."
				 ; putStrLn "Usage: hexup xqueryexpr srctree destfile."
				 ; putStrLn "Using standard arguments."
				 ; return False
				}

-- Interprets the expressions on the given XML file.
doexprs :: XmlTree -> Env -> File -> [ExprSingle] -> IO (Either TupleStream (Env,File))
doexprs _ env file [] = return (Right (env,file))
doexprs srctree env file (e:es) = do
    v <- doexpr srctree env file e
    case v of
        Right (env',file') -> doexprs srctree env' file' es


doexpr :: XmlTree -> Env -> File -> ExprSingle -> IO (Either TupleStream (Env,File))
doexpr srctree env file expr = case expr of
	(FLWORExpr flwor) -> doflwors srctree env file flwor
	(DeleteExpr delTarget) -> dodelete srctree env file delTarget
	(InsertExpr src targChoice targ) -> doinsert srctree env file src targChoice targ
	(ReplaceExpr valReplace targ with) -> doreplace srctree env file valReplace targ with
	(RenameExpr targ newname) -> dorename srctree env file targ newname


-- FLWORExpr
doflwors :: XmlTree -> Env -> File -> [FLWORExpr] -> IO (Either TupleStream (Env,File))
doflwors srctree env file [] = return (Right (env,file))
doflwors srctree env file (f:fs) = do
    v <- doflwor srctree env file f
    case v of
        Right (env',file') -> doflwors srctree env' file' fs
        _ -> doflwors srctree env file fs

dorename :: XmlTree -> Env -> File -> TargetExpr -> NewNameExpr -> IO (Either TupleStream (Env,File))
dorename srctree env file targ newname
		= do { (nrIDs, idtree) <- return $ Update.insertIDs 0 srctree
			 ; targtree <- doxpath idtree env file targ
			 ; updatedDoc <- return (Update.removeIDs (Update.rename newname targtree idtree))
			 ; return $ Right $ (env, xshow [updatedDoc])
			 }

doreplace :: XmlTree -> Env -> File -> Bool -> TargetExpr -> XPathExpr -> IO (Either TupleStream (Env,File))
doreplace srctree env file valReplace targ with
		= do { (nrIDs, idtree) <- return $ Update.insertIDs 0 srctree
			 ; targtree <- doxpath idtree env file targ
			 ; withtree <- doxpath idtree env file with
			 ; updatedDoc <- return (Update.removeIDs  (Update.replace valReplace withtree targtree idtree))
			 ; return $ Right $ (env, xshow [updatedDoc])
			 }

dodelete :: XmlTree -> Env -> File -> TargetExpr -> IO (Either TupleStream (Env,File))
dodelete srctree env file delTarget
		= do { (nrIDs, idtree) <- return $ Update.insertIDs 0 srctree
			 ; y <- doxpath idtree env file delTarget
			 ; updatedDoc <- return $  Update.removeIDs (Update.removeTrees y idtree)
			 ; return (Right (env, xshow [updatedDoc]))
			 }

doinsert :: XmlTree -> Env -> File -> SourceExpr -> InsertExprTargetChoice -> TargetExpr -> IO (Either TupleStream (Env, File))
doinsert srctree env file src targChoice targ
		= do { (nrIDs, idtree) <- return $ Update.insertIDs 0 srctree
			 ; srctree <- doxpath idtree env file src
			 ; targtree <- doxpath idtree env file targ
			 ; funct <- return $ case targChoice of {
				 					  Before  -> Update.insertBefore
									; After   -> Update.insertAfter
									; AsFirst -> Update.insertAsFirst
									; AsLast  -> Update.insertAsLast
									}
			 ; updatedDoc <- return $ Update.removeIDs (Update.insertNodes funct srctree targtree idtree)
			 ; return (Right (env, xshow [updatedDoc]))
			 }


doflwor :: XmlTree -> Env -> File -> FLWORExpr -> IO (Either TupleStream (Env,File))
doflwor srctree env file flwor = case flwor of
    -- Executes a for loop
    (ForClause id e) -> do { y <- doxpaths srctree env file e
                           ; return (Right ((M.insert id (mkNode (XTag (mkName "for") []) (concat y)) (fst env),(snd env) ++  [(id,'f')]) ,file))
						   }
    -- Saves a variable in teh environment.
    (LetClause id e) -> do { y <- doxpaths srctree env file e
                           ; return (Right ((M.insert id (mkNode (XTag (mkName "let") []) (concat y)) (fst env),(snd env) ++ [(id,'l')]) ,file))
							}
	-- an orderby-clause is parsed like this: it has to begin with an identifier, possibly a list of children
	-- in a tree-descending manner and may be finished with an attribute (which has to be preceded by an @)
	-- it can only refer to variables bound to a for clause, because it makes sense only in this case
	-- the help function sortlist re-sorts the children belonging to this for-variable
	-- the function ordering steps down in the tree to find the element after which the ordering should take place
    (OrderByClause id steplist) -> do { (sortedenvmap, envlist) <- return $ (M.update (sortList steplist) id (fst env),(snd env))
									  ; return (Right ((sortedenvmap, envlist),file))
									  }
			where
				sortList steplist (NTree node succs) = Just (NTree node (L.sortBy (ordering steplist)  succs) )
				ordering [] (NTree _ [succ1]) (NTree _ [succ2]) = compare succ1 succ2
				ordering (('@':attname):xs) (NTree (XTag node1 atts1) _) (NTree (XTag node2 atts2) _) = ordering xs (getAttByName (mkName attname) atts1) (getAttByName (mkName attname) atts2)
				ordering (x:xs) (NTree _ succ1) (NTree _ succ2) = ordering xs (getChildrenByName (mkName x) succ1) (getChildrenByName (mkName x) succ2)
				ordering _ _ _ = error "Unsupported or invalid orderBy-Statement"
				getChildrenByName s ((NTree (XTag name atts) succs):xs)
					| s == name 	= NTree (XTag name atts) succs
					| otherwise 	= getChildrenByName s xs
				getChildrenByName s (x:xs) = getChildrenByName s xs
				getChildrenByName _ _ = error "Unsupported or invalid orderBy-Statement"
				getAttByName attname ((NTree (XAttr name) succs):xs)
					| attname == name = NTree (XAttr name) succs
					| otherwise 	  = getAttByName attname xs
				getAttByName _ _ = error "Unsupported or invalid orderBy-statement"
    (ReturnClause e) -> do
        res <- return $ concat (replaceSymbols env [e])
        return (Right (env, "<result>" ++ res ++ "</result>"))
    --WhereClause -> undefined


-- replaces the symbols in a string by their values from the symbol
-- table
replaceSymbols :: Env -> [String] -> [String]
replaceSymbols (symtable, []) s = s
replaceSymbols (symbtable, ((symbol, forOrLet ):xs) ) s
-- case let clause
	| forOrLet == 'l' =	replaceSymbols (symbtable,xs) (replaceInList s ('$':symbol)  (xshow (getChildren (mapLookup symbtable symbol))))
-- case for clause
	| forOrLet == 'f' = replaceSymbols (symbtable,xs) (replaceForInList (length forStream) (stretchList (length forStream) s) ('$' : symbol) forStream)
		where forStream = getChildren (mapLookup symbtable symbol)

-- stretchList n list creates a new list, replicating each element of the old list n times
stretchList :: Int -> [a] -> [a]
stretchList _ [] = []
stretchList n (x:xs) = replicate n x ++  stretchList n xs

-- the main function of the for replacing mechanism. It iterates trough the list - which was
-- stretched before by the function stretchList - and substitutes each occurence of the
-- symbol by the appropriate entry in the symbol table. So if our for expression contains
-- (1,2) and the list is 4 elements long, it will be replaced in the order (1,2,1,2)
replaceForInList :: Int -> [String] -> String -> XmlTrees -> [String]
replaceForInList _ [] _ _ = []
replaceForInList 1 (x:xs) symb trees = (replace x symb (xshow [(last trees)])) : replaceForInList (length trees) xs symb trees
replaceForInList n (x:xs) symb trees = (replace x symb (xshow [trees !! ((length trees)-n)])) : replaceForInList (n-1) xs symb trees


-- replaces an occurence of a string with another in a list of strings
replaceInList [] _ _ = []
replaceInList (x:xs) r b = (replace x r b) : (replaceInList xs r b)

-- looks up a key in the value. Function to circumvent the error
-- handling ;-) Error handling to be added
mapLookup :: (Ord k, Show k) => M.Map k a -> k -> a
mapLookup m k = case (M.lookup k m) of
                    Just result -> result
                    Nothing     -> error $ "mapLookup: Key " ++ show k
                                         ++ " does not exist in map."

-- replace function, taken from
-- http://bluebones.net/2007/01/replace-in-haskell/
-- replace s r b replaces r by b in s
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace _ [] list = list
replace s find repl =
    if take (length find) s == find
        then repl ++ (replace (drop (length find) s) find repl)
        else [head s] ++ (replace (tail s) find repl)


doxpaths :: XmlTree -> Env -> File -> [XPathExpr] -> IO [Value]
doxpaths _ env file []     = return []
doxpaths srctree env file (x:xs) = do
    y <- doxpath srctree env file x
    z <- doxpaths srctree env file xs
    return (y:z)

-- doxpath returns now the result of an xpath evaluation as Value (XmlTree)
doxpath :: XmlTree -> Env -> File -> XPathExpr -> IO Value
doxpath srctree env file xexpr = do
    case head xexpr of
        '<' -> return $ xread xexpr
        _   -> return (getXPath xexpr srctree)
