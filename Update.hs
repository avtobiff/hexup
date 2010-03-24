module Update where

import Text.XML.HXT.DOM
import Text.XML.HXT.DOM.QualifiedName
import Text.XML.HXT.Parser
import Text.XML.HXT.XPath


path = "examples/test.xml"

{-
-- test routines
main :: IO ()
main = do { (xmldoc, errs, rc) <- getXmlDocument [(a_validate, "0")] path
		  ; (resid, resdoc) <- return $ insertIDs 0 xmldoc
		  --; deleted <- return $ removeTrees (getXPath "/recipes/recipe/ingredient/@amount" resdoc) resdoc
		  --; putStrLn $ show $ getXPath "recipes/recipe/ingredient/@amount" resdoc
		  --; putStrLn $ show [resdoc]
		  ; tree1 <- return $ getXPath "/recipes/recipe/ingredient[@amount < 3]" resdoc
		  --; resTree <- return $ renameNodeInTree "new" (head tree2) resdoc
		  ; tree2 <- return $ getXPath "/recipes/recipe/ingredient[@amount=0.5]" resdoc
		  ; resTree <- return $ replaceValue [test3] (head tree2) resdoc
		  ; putStrLn $ xshow [test3]
		  ; putStrLn $ xshow tree2
		  --; putStrLn $ xshow $ [removeIDs resTree]
		  ; putStrLn $ show $ getXPath "<one/>" resdoc
		  --; putStrLn $ show $ allAtts tree1
		  --; (errs2, ret) <- putXmlDocument [(a_indent, "1")] "-" (removeIDs resTree)
		  ; putStrLn "";
		  }

test1 :: XmlTree
test1 = NTree (XTag (mkName "foo") [test2]) []
test2 = NTree (XAttr (QN {namePrefix = "", localPart = "niceatt", namespaceUri = ""})) [NTree (XCmt "1000") [], NTree (XText "3") []]
test3 = NTree (XText "FOOOO") [NTree (XCmt "10000") []]
-}

replace :: Bool -> XmlTrees -> XmlTrees -> XmlTree -> XmlTree
replace valueOrNot src (x:[]) doc
		| valueOrNot	= replaceValue src x doc
		| otherwise		= replaceN src x doc
replace _ _ _ _ = error "Invalid replace statement. Only one node at a time can be replaced."

-- replace new target doc replaces node target with the trees in new in document doc
replaceN :: XmlTrees -> XmlTree -> XmlTree -> XmlTree
replaceN newTrees target doc = removeNode target (insertAfter newTrees target doc)


replaceValue :: XmlTrees -> XmlTree -> XmlTree -> XmlTree
replaceValue s targ@(NTree (XTag node attr) succs) doc = replaceValueInTag s targ doc

replaceValueInTag :: XmlTrees -> XmlTree -> XmlTree -> XmlTree
replaceValueInTag s targ doc@(NTree docnode docsuccs)
	| targ == doc		= NTree docnode ((deleteTextNode docsuccs) ++ s)
	| otherwise 		= NTree docnode (replaceValueInTag' s targ docsuccs)

replaceValueInTag' :: XmlTrees -> XmlTree -> XmlTrees -> XmlTrees
replaceValueInTag' _ _ [] = []
replaceValueInTag' s targ (x:xs) = (replaceValueInTag s targ x) : (replaceValueInTag' s targ xs)

deleteTextNode :: XmlTrees -> XmlTrees
deleteTextNode [] = []
deleteTextNode ( (NTree (XText s) succs) : xs ) = xs
deleteTextNode (x:xs) = x : (deleteTextNode xs)

insertNodes f t (x:[]) d = f t x d
insertNodes _ _ _ _ = error "Invalid insert operation. Can only be done into one node."


-- insertBefore new target doc inserts the nodes from the list new
-- before the node target in the tree doc, all of the arguments have 
-- to be trees equipped with an ID by function insertIDs
insertBefore :: XmlTrees -> XmlTree -> XmlTree -> XmlTree
insertBefore newTrees target doc = insertBeforeOrAfter insertBeforeInList newTrees target doc

-- insertAfter new target doc inserts the nodes from the list new
-- after the node target in the tree doc, all of the arguments have 
-- to be trees equipped with an ID by function insertIDs
insertAfter :: XmlTrees -> XmlTree -> XmlTree -> XmlTree
insertAfter newTrees target doc = insertBeforeOrAfter insertAfterInList newTrees target doc

-- help higher-order-function
insertBeforeOrAfter beforeOrAfter newTrees target doc@(NTree (XTag node atts) succs) 
	| allAtts newTrees && target `elem` atts = NTree (XTag node (beforeOrAfter newTrees target atts)) succs
	| target `elem` succs = NTree (XTag node atts) (beforeOrAfter newTrees target succs)
	| otherwise           = NTree (XTag node atts) (insertBeforeOrAfter' beforeOrAfter newTrees target succs)
insertBeforeOrAfter beforeOrAfter newTrees target doc@(NTree node succs)
	| allAtts newTrees    = doc
	| target `elem` succs = NTree node (beforeOrAfter newTrees target succs)
	| otherwise           = NTree node (insertBeforeOrAfter' beforeOrAfter newTrees target succs)


insertBeforeOrAfter' _ _ _ [] = []
insertBeforeOrAfter' beforeOrAfter newTrees target (x:xs) = (insertBeforeOrAfter beforeOrAfter newTrees target x) : (insertBeforeOrAfter' beforeOrAfter newTrees target xs)

-- insertBeforeInList newl el oldl inserts newl into oldl before element el
insertBeforeInList :: Eq a => [a] -> a -> [a] -> [a]
insertBeforeInList newList breakEl list = p1 ++ newList ++ p2
	where (p1, p2) = break (== breakEl) list

insertAfterInList :: Eq a => [a] -> a -> [a] -> [a]
insertAfterInList newList breakEl list = p1 ++ [(head p2)] ++ newList ++ (tail p2)
	where (p1, p2) = break (== breakEl) list

-- insertAsFirst new target doc inserts the nodes from the list new
-- into the node target in the tree doc, all of the arguments have
-- to be trees equipped with an ID by function insertIDs
insertAsFirst :: XmlTrees -> XmlTree -> XmlTree -> XmlTree
insertAsFirst new targ doc 
	| allAtts new 	= insertAsFirstOrLast (\x -> \y -> y ++ x) new targ doc
	| otherwise		= insertAsFirstOrLast (\x -> \y ->  (head x) : (y ++ (tail x))) new targ doc 
												-- insert new node after ID comment node


-- insertAsLast new target doc inserts the nodes from the list new
-- into the node target in the tree doc, all of the arguments have
-- to be trees equipped with an ID by function insertIDs
insertAsLast :: XmlTrees -> XmlTree -> XmlTree -> XmlTree
insertAsLast new target doc = insertAsFirstOrLast (++) new target doc 


insertAsFirstOrLast :: (XmlTrees -> XmlTrees -> XmlTrees) -> XmlTrees -> XmlTree -> XmlTree -> XmlTree
insertAsFirstOrLast f new targ doc@(NTree (XTag name attr) succs) 
	| allAtts new && targ == doc	= NTree (XTag name (f attr new)) succs
	| targ == doc					= NTree (XTag name attr) (f succs new)
	| otherwise						= NTree (XTag name attr) (insertAsFirstOrLast' f new targ succs)
insertAsFirstOrLast f new targ doc@(NTree node succs)
	| targ == doc	= NTree node (f succs new)
	| otherwise 	= NTree node (insertAsFirstOrLast' f new targ succs)

insertAsFirstOrLast' _ _ _ [] = []
insertAsFirstOrLast' f new targ (x:xs) = (insertAsFirstOrLast f new targ x) : (insertAsFirstOrLast' f new targ xs)

-- allAtts tree checks if every node in the tree is an attribute node
allAtts :: XmlTrees -> Bool
allAtts [] = True
allAtts ((NTree (XAttr name) succs) : xs) = allAtts xs
allAtts _ = False


-- insertIDs inserts an unique ID into every node of the tree
-- so every node becomes unique. before any updating function is
-- executed, insertIDs has to be called to ensure the proper
-- processing of the tree. It should be called with the first
-- ID that should be set and it returns the last used ID + the
-- changed XmlTree
insertIDs :: Int -> XmlTree -> (Int, XmlTree)
insertIDs n (NTree (XTag name attlist) succs) = (idsucc,  (NTree (XTag name resatt) ((NTree (XCmt ((show n))) []) : ressucc )) )
	where { (idatt, resatt) = insertIDs' (n+1) attlist
	      ; (idsucc, ressucc) = insertIDs' (idatt+1) succs
		  }
insertIDs n (NTree (XPi name attlist) succs) = (idsucc,  (NTree (XPi name resatt) ((NTree (XCmt (show n)) []) : ressucc )) )
	where { (idatt, resatt) = insertIDs' (n+1) attlist
	      ; (idsucc, ressucc) = insertIDs' (idatt+1) succs
		  }
insertIDs n (NTree node succs) = (idsucc, NTree node ((NTree (XCmt (show n)) []) : ressucc) )
	where (idsucc, ressucc) = insertIDs' (n+1) succs


insertIDs' :: Int -> XmlTrees -> (Int, XmlTrees)
insertIDs' n [] =(n,  [])
insertIDs' n (x:xs) = (id2 , res : res2       )
	where {(id, res) = insertIDs n x
		  ; (id2, res2) = insertIDs' (id+1) xs
		  }

-- removeIDs removes the IDs inserted by insertIDs from the tree
-- and should be called before a tree is printed out to the screen
-- or to a file
-- to prevent from deleting content nodes, it checks if the first
-- component of every list is a comment. This doesn't offer absolute
-- guarantee that we really delete the ID node, but for our 
-- purposes it's enough
removeIDs :: XmlTree -> XmlTree
removeIDs (NTree node []) = NTree node []
removeIDs (NTree (XTag name attlist) ((NTree (XCmt _) _):rest)) = NTree (XTag name (removeIDs' attlist)) (removeIDs' rest) 
removeIDs (NTree (XTag name attlist) succs) = NTree (XTag name (removeIDs' attlist)) (removeIDs' succs) 
removeIDs (NTree (XPi name attlist) ((NTree (XCmt _) _):rest)) = NTree (XPi name (removeIDs' attlist)) (removeIDs' rest) 
removeIDs (NTree (XPi name attlist) succs) = NTree (XPi name (removeIDs' attlist)) (removeIDs' succs) 
removeIDs (NTree node ((NTree (XCmt _) _):rest)) = NTree node (removeIDs' rest)
removeIDs (NTree node rest) = NTree node (removeIDs' rest)

removeIDs' :: XmlTrees -> XmlTrees
removeIDs' [] = []
removeIDs' (x:xs) = (removeIDs x) : (removeIDs' xs)

-- this function removes more than one tree from an XML document
-- should only be called after applying insertIDs to the document
removeTrees :: XmlTrees -> XmlTree -> XmlTree
removeTrees [] t = t
removeTrees (x:xs) t = removeTrees xs (removeNode x t)


--removeNode s t removes every occurence of s from t
--although the function is called "removeNode", it expects two trees as input.
-- should only be called after applying insertIDs to the document
removeNode :: XmlTree -> XmlTree -> XmlTree
removeNode _ a@(NTree r []) = a
-- special treatment for XTag and XPi constructors because here also the attribute list has to be searched
-- for elements to remove.
removeNode n (NTree (XTag name attlist) succs) = NTree (XTag name (deleteAll n attlist)) (removeNodes n (deleteAll n succs))
removeNode n (NTree (XPi name attlist) succs) = NTree (XPi name (deleteAll n attlist)) (removeNodes n (deleteAll n succs))
removeNode n (NTree r succs) = NTree r (removeNodes n (deleteAll n succs))

removeNodes :: XmlTree -> XmlTrees -> XmlTrees
removeNodes _ [] = []
removeNodes n (x:xs) = (removeNode n x) : (removeNodes n xs)


-- deletes all occurences of an element from a list
-- (The standard libraries seem only to provide routines to
-- delete the first occurence, but elements in an XML document
-- can repeat)
deleteAll :: Eq a => a -> [a] -> [a]
deleteAll _ [] = []
deleteAll y (x:xs) | x == y = deleteAll y xs
				   | otherwise = x : (deleteAll y xs)

rename :: String -> XmlTrees -> XmlTree -> XmlTree
rename s (x:[]) doc = renameNodeInTree s x doc
rename _ _ _ 		= error "Invalid rename statement. Only one node at a time can be renamed."

-- renameNodeInTree s targ source 
-- renames the node targ inside the tree source with s
renameNodeInTree :: String -> XmlTree -> XmlTree -> XmlTree
renameNodeInTree s targ src@(NTree (XAttr name) succs) 
	| targ == src  =  NTree (renameNode s (XAttr name)) succs
	| otherwise	   = NTree (XAttr name) (renameNodeInTree' s targ succs)
renameNodeInTree s targ src@(NTree (XTag name attlist) succs) 
	| targ == src  =  NTree (renameNode s (XTag name attlist)) succs
	| otherwise	   = NTree (XTag name (renameNodeInTree' s targ attlist)) (renameNodeInTree' s targ succs)
renameNodeInTree s targ src@(NTree (XPi name attlist) succs) 
	| targ == src  =  NTree (renameNode s (XPi name attlist)) succs
	| otherwise	   = NTree (XPi name (renameNodeInTree' s targ attlist)) (renameNodeInTree' s targ succs)
renameNodeInTree s targ src@(NTree node succs) = NTree node (renameNodeInTree' s targ succs)

renameNodeInTree' :: String -> XmlTree -> XmlTrees -> XmlTrees
renameNodeInTree' _ _ [] = []
renameNodeInTree' s targ (x:xs) = (renameNodeInTree s targ x) : (renameNodeInTree' s targ xs)



-- convenient function to rename nodes, creates a QName with empty
-- prefix and URI
renameNode :: String -> XNode -> XNode
renameNode newName node = qRenameNode (QN {namePrefix="", localPart=newName, namespaceUri=""}) node



-- function to rename a node
qRenameNode :: QName -> XNode -> XNode
qRenameNode newName (XAttr _) = XAttr newName
qRenameNode newName (XTag _ attlist) = XTag newName attlist
qRenameNode newName (XPi _ attlist) = XTag newName attlist
qRenameNode _ _ = error "renamed node must be an attribute, element or document processing node"



