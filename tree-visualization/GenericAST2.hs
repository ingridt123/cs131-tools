module GenericAST2 where

import Data.Data
import Data.Typeable
import Data.List ( elemIndices )
import Data.Generics.Text ( gshow )
import Data.Generics.Schemes
import AST
import Dot

-- | Constants
name :: String -> Maybe Id
name n = Just (convertToId n)

rootId :: [Char]
rootId = "1"

type StatementS = [Statement] -> [Statement]

-- METHOD 2: Build AST from deconstructing data type

-- | Build AST from any data type
dataAST :: Data d => d -> DotGraph
dataAST d = DotGraph NonStrict Directed (name "") (dataASTStatements rootId d [])

-- | Build list of statements for AST
dataASTStatements :: Data d => [Char] -> d -> StatementS
dataASTStatements nextId d = 
      (createNodeS nextId . showConstr . toConstr $ d)              -- Create parent node
    . (foldr (.) id . dataASTEdges nextId $ childIds)               -- Create edges parent node --> 0 or more child(ren) node(s)
    . (foldr (.) id . subtrees $ d)                                 -- Recurse on the subterms to create subtrees
    where childNum = if typeOf d == typeOf "Hello" then 1 else glength d
          childIds = dataASTChildIds nextId childNum
          subtrees d = if typeOf d == typeOf "Hello"        -- TODO: BETTER WAY OF DOING THIS!!!
                then [createNodeS (head childIds) (gshow d)]
                else map (\i -> gmapQi i (dataASTStatements (childIds !! i)) d) [0,1..(glength d - 1)]
          -- Apply gmapQi / dataASTStatements to every subterm of d with childIds[i]

    -- . (foldr (.) id . gmapQ (dataASTStatements nextId) $ d)
    -- doesn't work because need to iterate through childIds

-- | Version of createNode that return StatementS instead of Statement
createNodeS :: String -> String -> StatementS
createNodeS id label = (++) [createNode id label]

-- | Version of createEdge that return StatementS instead of Statement
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