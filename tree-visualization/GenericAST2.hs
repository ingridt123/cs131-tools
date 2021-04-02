module GenericAST2 where

import Data.Data
import Data.List ( elemIndices )
import Data.Generics.Text ( gshow )
import Data.Generics.Schemes
import AST
import Dot

-- | Constants
name n = Just (convertToId n)
rootId = "1"

type StatementS = [Statement] -> [Statement]

-- METHOD 2: Build AST from deconstructing data type

-- | Build AST from any data type
dataAST :: Data d => d -> DotGraph
dataAST d = DotGraph NonStrict Directed (name "") (dataASTStatements rootId d [])

-- | Build list of statements for AST
dataASTStatements :: Data d => [Char] -> d -> StatementS
dataASTStatements nextId d = 
      (createNodeS nextId . showConstr . toConstr $ d) 
    . (foldr (.) id . dataASTEdges nextId $ dataASTChildIds nextId (glength d))
    . (foldr (.) id . gmapQ (dataASTStatements nextId) $ d)
    -- can't be list of statements --> gmapQ combines into [Statement]

-- gshows = ( \t ->
--                 showChar '('                                              -- Prepends output with (          :: ShowS
--               . (showString . showConstr . toConstr $ t)                  -- Adds constructor name to output :: ShowS
--               . (foldr (.) id . gmapQ ((showChar ' ' .) . gshows) $ t)    -- Adds subterms to output         :: ShowS
--               . showChar ')'                                              -- Appends output with )           :: ShowS

createNodeS :: String -> String -> StatementS
createNodeS id label =  (++) [createNode id label]
-- createNodeS id label =  [createNode id label] (++) doesn't work?

createEdgeS :: String -> String -> StatementS
createEdgeS id1 id2 = (++) [createEdge id1 id2]

-- | Add edges from parent to children in AST
-- TODO: cleaner code?
dataASTEdges :: String -> [String] -> [StatementS]
dataASTEdges _ [] = []
dataASTEdges nextId (id : ids) = createEdgeS nextId id : dataASTEdges nextId ids
-- dataASTEdges :: String -> [String] -> [Statement]
-- dataASTEdges _ [] = []
-- dataASTEdges nextId (id : ids) = createEdge nextId id : dataASTEdges nextId ids

-- Build list of child ids
-- TODO: cleaner code?
dataASTChildIds :: String -> Int -> [String]
dataASTChildIds _ 0 = []
dataASTChildIds nextId i = dataASTChildIds nextId (i-1) ++ [nextId ++ "." ++ show i]


-- -- | Build list of statements for AST
-- dataASTStatements :: Data d => [Char] -> d -> [Statement]
-- dataASTStatements nextId d =
--     createNode nextId (showConstr (toConstr d)) :        -- Create parent node
--     dataASTEdges nextId childIds ++                      -- Add edges from parent to children
--     dataASTChildNodes childIds constrArgs                -- 
--     -- TODO: child node for each field
--     -- TODO: recurse dataASTStatements on all childIds and all constrArgs
--     -- concatMap (\i -> dataASTStatements (childIds !! i) (parentLists !! i)) rangeList
--     where constrArgs = dataASTConstrArgs d
--           childIds = dataASTChildIds nextId (length constrArgs)





-- -- TODO: can constrArgs contain different types? or normalize to one type?
-- dataASTConstrArgs :: Data d => d -> [[Char]] -- TODO: change data type [[Char]]
-- dataASTConstrArgs = gmapQ gshow -- TODO

-- -- Add child nodes to AST
-- -- dataASTChildNodes :: [[Char]] -> [[Char]] -> [Statement]  -- TODO: change data type [[Char]]
-- -- dataASTChildNodes [] _ = []
-- -- dataASTChildNodes (id : ids) (arg : args) = createNode id arg : dataASTChildNodes ids args
-- -- take first id to create node for first field
-- -- TODO: add showConstr?