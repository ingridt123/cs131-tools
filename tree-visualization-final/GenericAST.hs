----------------------------------------------------------------------------
-- |
-- Module      :  GenericAST
-- Copyright   :  (c) Ingrid Tsang, 2021 ?????
-- License     :  GPL    ?????
-- 
-- Maintainer  :  itsang@hmc.edu
-- Stability   :  experimental
-- Portability :  TODO?
--
-- This module builds abstract syntax trees for generic data.
--
-- (TODO link) ???
-----------------------------------------------------------------------------

module GenericAST (

      -- * Generic AST
      gAST, gStoreAST

) where

------------------------------------------------------------------------------

import Data.Data ( Data(toConstr, gmapQi), showConstr, typeOf )
import Data.Generics.Text ( gshow )
import Data.Generics.Schemes ( glength )
import Data.List ( elemIndices )
import Data.Text (pack)
import Dot
import Control.Concurrent
import System.FilePath.Posix
import System.Process
import System.Directory

------------------------------------------------------------------------------

-- [Attribute (convertToId "label") (convertToId label)] TODO: FIX THIS??

-- | Build abstract syntax tree (AST) from any data type
gAST :: Data d => d           -- ^ Data instance, must derive (Data)
               -> Bool        -- ^ True to expand String as [Char] in AST, False otherwise
               -> DotGraph    -- ^ Representation of dot graph
gAST d expandStr = 
      DotGraph NonStrict Directed (Just (convertToId "")) (gASTStatements "1" d expandStr [])


-- Store AST dot and png files of dot graph in dirName as fileName
gStoreAST :: Data d => d                  -- ^ Data instance, must derive (Data)
                    -> Bool               -- ^ True to expand String as [Char] in AST, False otherwise
                    -> FilePath           -- ^ Name of directory, set to "" to store in current directory
                    -> FilePath           -- ^ Base name of file
                    -> IO ProcessHandle
gStoreAST d expandStr dirName fileName = do
  createDirectoryIfMissing True dirName
  encodeToFile dotFile (gAST d expandStr)
  runCommand ("dot " ++ dotFile ++ " -Tpng > " ++ pngFile)
  where dotFile = targetDot dirName fileName
        pngFile = targetPng dirName fileName

------------------------------------------------------------------------------
-- GENERIC AST HELPER FUNCTIONS
------------------------------------------------------------------------------

-- | Difference list of statements for DotGraph
type StatementS = [Statement] -> [Statement]

-- | Build list of statements for AST
gASTStatements :: Data d => [Char]        -- ^ Id of next node in AST
                         -> d             -- ^ Generic data, must derive (Data)
                         -> Bool          -- ^ True to expand String as [Char] in AST, False otherwise
                         -> StatementS    -- ^ Difference list of statements for AST
gASTStatements nextId d expandStr = 
      if (typeOf d == typeOf "") && not expandStr
            then createNodeS nextId (gshow d)
            else (createNodeS nextId . showConstr . toConstr $ d)
                  . (foldr (.) id . gASTEdges nextId $ childIds)
                  . (foldr (.) id . subtrees $ d)
    where childNum   = glength d
          childIds   = gASTChildIds nextId childNum
          subtrees d = map (\i -> gmapQi i (gASTStatements (childIds !! i)) d expandStr) [0,1..(glength d - 1)]


-- | Build list of edge statements for AST from parentId to ids
gASTEdges :: String           -- ^ Id of parent node
          -> [String]         -- ^ List of ids of child nodes
          -> [StatementS]     -- ^ Difference list of edge statements
gASTEdges _ [] = []
gASTEdges parentId (id : ids) = createEdgeS parentId id : gASTEdges parentId ids


-- | Build list of child ids for node with parentId in AST
gASTChildIds :: String        -- ^ Id of parent node
             -> Int           -- ^ Number of child nodes remaining
             -> [String]      -- ^ List of child ids
gASTChildIds _ 0 = []
gASTChildIds parentId i = gASTChildIds parentId (i-1) ++ [parentId ++ "." ++ show i]

------------------------------------------------------------------------------
-- DOT LIBRARY HELPER FUNCTIONS
------------------------------------------------------------------------------

-- Convert to Id data type used by Dot library
convertToId :: String -> Id
convertToId id = Id (pack id)

-- Convert to NodeId data type used by Dot library
convertToNodeId :: String -> NodeId 
convertToNodeId id = NodeId (convertToId id) Nothing

-- Create node statement, represented by StatementNode in Dot library
createNode :: String -> String -> Statement
createNode id label = 
    StatementNode $ NodeStatement (convertToNodeId id) 
                                  [Attribute (convertToId "label") (convertToId (" " ++ label ++ " "))]

-- | Version of createNode that returns StatementS instead of Statement
createNodeS :: String -> String -> StatementS
createNodeS id label = (++) [createNode id label]

-- Create edge statement, represented by StatementEdge in Dot library
createEdge :: String -> String -> Statement 
createEdge id1 id2 =
    StatementEdge $ EdgeStatement (ListTwo (EdgeNode (convertToNodeId id1)) 
                                           (EdgeNode (convertToNodeId id2)) 
                                           [])
                                  []

-- | Version of createEdge that return StatementS instead of Statement
createEdgeS :: String -> String -> StatementS
createEdgeS id1 id2 = (++) [createEdge id1 id2]


------------------------------------------------------------------------------
--  FILE HELPER FUNCTIONS
------------------------------------------------------------------------------
targetDot :: String -> String -> FilePath
targetDot "" fileName = addExtension fileName "dot"
targetDot dirName fileName = dirName </> addExtension fileName "dot"

targetPng :: String -> String -> FilePath
targetPng "" fileName = addExtension fileName "png"
targetPng dirName fileName = dirName </> addExtension fileName "png"