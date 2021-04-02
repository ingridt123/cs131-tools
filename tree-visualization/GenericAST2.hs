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

    -- . ((.) . (\i -> gmapQi i (dataASTStatements ["1","2","3"] !! i)) $ d)
    -- . (foldr (.) id . map (\i -> gmapQi i (dataASTStatements ["1","2","3"] !! i)) $ d)

    

createNodeS :: String -> String -> StatementS
createNodeS id label =  (++) [createNode id label]
-- createNodeS id label =  [createNode id label] (++) doesn't work?

createEdgeS :: String -> String -> StatementS
createEdgeS id1 id2 = (++) [createEdge id1 id2]

-- | Add edges from parent to children in AST
dataASTEdges :: String -> [String] -> [StatementS]
dataASTEdges _ [] = []
dataASTEdges nextId (id : ids) = createEdgeS nextId id : dataASTEdges nextId ids

-- Build list of child ids
dataASTChildIds :: String -> Int -> [String]
dataASTChildIds _ 0 = []
dataASTChildIds nextId i = dataASTChildIds nextId (i-1) ++ [nextId ++ "." ++ show i]