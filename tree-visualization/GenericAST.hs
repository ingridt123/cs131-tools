module GenericAST where

import Data.Data
import Data.List
import Data.Generics.Text -- for gshow
import AST
import Dot
import TestData ( Exp(Num, Times, Plus) )

name n = Just (convertToId n)
rootId = "1"

run = createDotFile "genericAST-test8" (dataAST (Times (Num 10) (Times (Num 3) (Plus (Num 1) (Num 11)))))

dataAST :: Data d => d -> DotGraph
dataAST d = DotGraph NonStrict Directed (name "") (dataASTStatements rootId (showConstr (toConstr d) : gmapQ gshow d))

-- ["Parent1", "(Child1.1 (...))", "(Child1.2 (...))", ...]
dataASTStatements :: String -> [String] -> [Statement]
dataASTStatements nextId (constr : constrArgs) =
    createNode nextId constr :
    dataASTEdges nextId childIds ++
    concatMap (\i -> dataASTStatements (childIds !! i) (parentLists !! i)) rangeList
    -- apply dataASTParentList to every string in constrArgs -> list of [String]
    -- apply dataASTStatements to next childId + every list in list of [String]
    -- for every element in constrArgs, run dataASTStatements
    where rangeList = [0,1..(length constrArgs - 1)]
          childIds = dataASTChildIds nextId (length constrArgs)
          parentLists = map dataASTParentList constrArgs
    

-- dataASTStatements :: Data d => [Char] -> d -> [Statement]
-- dataASTStatements nextId d =
--     createNode nextId (showConstr (toConstr d)) :
--     dataASTEdges nextId childIds ++        
--     dataASTChildNodes childIds constrArgs             -- TODO: child node for each field
--     -- TODO: recurse dataASTStatements on all childIds and all constrArgs
--     where constrArgs = dataASTConstrArgs d
--           childIds = dataASTChildIds nextId (length constrArgs)

-- can constrArgs contain different types? or normalize to one type?
dataASTConstrArgs :: Data d => d -> [[Char]] -- TODO: change data type [[Char]]
dataASTConstrArgs = gmapQ gshow -- TODO
-- ["(Num (10))","(Times (Num (3)) (Plus (Num (1)) (Num (11))))"]

dataASTChildIds :: String -> Int -> [String]
dataASTChildIds _ 0 = []
dataASTChildIds nextId i = dataASTChildIds nextId (i-1) ++ [nextId ++ "." ++ show i]

dataASTEdges :: String -> [String] -> [Statement]
dataASTEdges _ [] = []
dataASTEdges nextId (id : ids) = createEdge nextId id : dataASTEdges nextId ids

dataASTParentList :: String -> [String]
dataASTParentList "(Num (10))" = ["Num", "(10)"]
dataASTParentList "(10)" = ["10"]
dataASTParentList "(Times (Num (3)) (Plus (Num (1)) (Num (11))))" = ["Times", "(Num (3))", "(Plus (Num (1)) (Num (11)))"]
dataASTParentList "(Num (3))" = ["Num", "(3)"]
dataASTParentList "(3)" = ["3"]
dataASTParentList "(Plus (Num (1)) (Num (11)))" = ["Plus", "(Num (1))", "(Num (11))"]
dataASTParentList "(Num (1))" = ["Num", "(1)"]
dataASTParentList "(1)" = ["1"]
dataASTParentList "(Num (11))" = ["Num", "(11)"]
dataASTParentList "(11)" = ["11"]
dataASTParentList _ = ["Hello"]
-- dataASTParentList constrArg = ["Num", "(10)"] -- TODO
    -- where (open1 : open2 : _) = elemIndices '(' constrArg
    --       (parent : )
-- "(Num (10))" --> ["Num", "(10)"]
-- "(Times (Num (3)) (Plus (Num (1)) (Num (11))))" --> ["Times", "(Num (3))", "(Plus (Num (1)) (Num (11)))"]

-- dataASTChildNodes :: [[Char]] -> [[Char]] -> [Statement]  -- TODO: change data type [[Char]]
-- dataASTChildNodes [] _ = []
-- dataASTChildNodes (id : ids) (arg : args) = createNode id arg : dataASTChildNodes ids args
-- take first id to create node for first field
-- TODO: add showConstr?