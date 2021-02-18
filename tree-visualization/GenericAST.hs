module GenericAST where

import Data.Data
import AST
import Dot
import TestData

name n = Just (convertToId n)
rootId = "1"

run = createDotFile "genericAST-test4" (dataAST (Times (Num 10) (Times (Num 3) (Plus (Num 1) (Num 11)))))

dataAST :: Data d => d -> DotGraph
dataAST d = DotGraph NonStrict Directed (name "") (dataASTStatements rootId d)

dataASTStatements :: Data d => [Char] -> d -> [Statement]
dataASTStatements nextId d =
    createNode nextId (showConstr (toConstr d)) :
    dataASTEdges nextId childIds ++        
    dataASTChildNodes childIds constrArgs             -- TODO: child node for each field
    -- TODO: recurse dataASTStatements on all childIds and all constrArgs
    where constrArgs = dataASTConstrArgs d
          childIds = dataASTChildIds nextId (length constrArgs)

-- can constrArgs contain different types? or normalize to one type?
dataASTConstrArgs :: Data d => d -> [[Char]] -- TODO: change data type [[Char]]
dataASTConstrArgs d = ["1", "2", "3"] -- TODO

dataASTChildIds :: [Char] -> Int -> [[Char]]
dataASTChildIds _ 0 = []
dataASTChildIds nextId i = dataASTChildIds nextId (i-1) ++ [nextId ++ "." ++ show i]

dataASTEdges :: [Char] -> [[Char]] -> [Statement]
dataASTEdges _ [] = []
dataASTEdges nextId (id : ids) = createEdge nextId id : dataASTEdges nextId ids

dataASTChildNodes :: [[Char]] -> [[Char]] -> [Statement]  -- TODO: change data type [[Char]]
dataASTChildNodes [] _ = []
dataASTChildNodes (id : ids) (arg : args) = createNode id arg : dataASTChildNodes ids args
-- take first id to create node for first field
-- TODO: add showConstr?