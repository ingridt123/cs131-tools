module GenericAST where

import Data.Data
import Data.List ( elemIndices )
import Data.Generics.Text ( gshow )
import AST
import Dot

-- | Constants
name n = Just (convertToId n)
rootId = "1"

-- | METHOD 1: Build AST from constructor string list

-- | Build AST from any data type
--   (showConstr (toConstr d) : gmapQ gshow d)
--   Parent1 (Child1.1 ...) (Child1.2 ...) ... --> ["Parent1", "(Child1.1 (...))", "(Child1.2 (...))", ...]
--   e.g. (Times (Num 10) (Times (Num 3) (Plus (Num 1) (Num 11)))) --> 
--        ["Times","(Num (10))","(Times (Num (3)) (Plus (Num (1)) (Num (11))))"]
dataAST :: Data d => d -> DotGraph
dataAST d = DotGraph NonStrict Directed (name "") (dataASTStatements rootId (dataASTStrList d))

-- | Build string list of data type for AST
dataASTStrList :: Data d => d -> [String]
dataASTStrList d = showConstr (toConstr d) : gmapQ gshow d

-- | Build list of statements for AST
dataASTStatements :: String -> [String] -> [Statement]          
dataASTStatements nextId (constr : constrArgs) =
    createNode nextId constr :
    dataASTEdges nextId childIds ++
    concatMap (\i -> dataASTStatements (childIds !! i) (parentLists !! i)) rangeList    -- apply dataASTParentList to every string in constrArgs -> list of [String]
                                                                                        -- apply dataASTStatements to next childId + every list in list of [String]
                                                                                        -- for every element in constrArgs, run dataASTStatements
    where rangeList = [0,1..(length constrArgs - 1)]
          childIds = dataASTChildIds nextId (length constrArgs)
          parentLists = map dataASTParentList constrArgs
    

-- | Add edges from parent to children in AST
dataASTEdges :: String -> [String] -> [Statement]
dataASTEdges _ [] = []
dataASTEdges nextId (id : ids) = createEdge nextId id : dataASTEdges nextId ids


-- | Get list of parents and children in AST for next recursive call
--   e.g. "(Num (10))" --> ["Num", "(10)"]
--        "(Times (Num (3)) (Plus (Num (1)) (Num (11))))" --> ["Times", "(Num (3))", "(Plus (Num (1)) (Num (11)))"]
dataASTParentList :: String -> [String]
dataASTParentList constrArg =
    let constrArg2 = if head constrArg == '('
                      then init . tail $ constrArg                 -- Remove outermost brackets
                      else constrArg
        spaceIndices = elemIndices ' ' constrArg2                  -- List of indexes of ' ' in constrArg2
    in if null spaceIndices
        then [constrArg2]
        else let (parent, children) = splitAt (head spaceIndices) constrArg2
             in parent : dataASTParentListHelper 0 False 0 (tail children)      -- Parent as first element, children as subsequent elements
-- dataASTParentList "(Num (10))" = ["Num", "(10)"]
-- dataASTParentList "(10)" = ["10"]
-- dataASTParentList "(Times (Num (3)) (Plus (Num (1)) (Num (11))))" = ["Times", "(Num (3))", "(Plus (Num (1)) (Num (11)))"]
-- dataASTParentList "(Num (3))" = ["Num", "(3)"]
-- dataASTParentList "(3)" = ["3"]
-- dataASTParentList "(Plus (Num (1)) (Num (11)))" = ["Plus", "(Num (1))", "(Num (11))"]
-- dataASTParentList "(Num (1))" = ["Num", "(1)"]
-- dataASTParentList "(1)" = ["1"]
-- dataASTParentList "(Num (11))" = ["Num", "(11)"]
-- dataASTParentList "(11)" = ["11"]
-- dataASTParentList _ = ["Hello"]
          
-- | Helper function to build list for children
--   e.g. "(Num (1)) (Num (11)))" --> ["(Num (1))", "(Num (11))"]
--        "(Num (3)) (Plus (Num (1)) (Num (11)))" --> ["(Num (3))","(Plus (Num (1)) (Num (11)))"]
--        "\"3\" (Leaf \"49\") (Leaf \"293\")" --> ["\"3\"", "(Leaf \"49\")", "(Leaf \"293\"))"]
dataASTParentListHelper :: Int -> Bool -> Int -> String -> [String]
dataASTParentListHelper _ _ _ "" = []                           -- No more characters to parse
dataASTParentListHelper paranCount inQuotes index children               
    | paranCount == 0 && index /= 0                             -- Create new child element if parans/quotes match
    = let (child, rest) = splitAt index children
          rest2 = if null rest then rest else tail rest
        in child : dataASTParentListHelper 0 False 0 rest2              
                                                                -- Update paranCount by parsing next character in string
    | length children > index && children !! index == '('
    = dataASTParentListHelper (paranCount + 1) inQuotes (index + 1) children
    | length children > index && children !! index == '"'
    = let newParanCount = if inQuotes then paranCount - 1 else paranCount + 1
        in dataASTParentListHelper newParanCount (not inQuotes) (index + 1) children
    | length children > index && children !! index == ')'
    = dataASTParentListHelper (paranCount - 1) inQuotes (index + 1) children
    | otherwise
    = dataASTParentListHelper paranCount inQuotes (index + 1) children   -- Must have more indexes because parantheses must match


-- METHOD 2: Build AST from deconstructing data type

-- | Build list of statements for AST
-- dataASTStatements :: Data d => [Char] -> d -> [Statement]
-- dataASTStatements nextId d =
--     createNode nextId (showConstr (toConstr d)) :        -- Create parent node
--     dataASTEdges nextId childIds ++                      -- Add edges from parent to children
--     dataASTChildNodes childIds constrArgs                -- 
--     -- TODO: child node for each field
--     -- TODO: recurse dataASTStatements on all childIds and all constrArgs
--     where constrArgs = dataASTConstrArgs d
--           childIds = dataASTChildIds nextId (length constrArgs)

-- TODO: can constrArgs contain different types? or normalize to one type?
dataASTConstrArgs :: Data d => d -> [[Char]] -- TODO: change data type [[Char]]
dataASTConstrArgs = gmapQ gshow -- TODO

-- Build list of child ids
dataASTChildIds :: String -> Int -> [String]
dataASTChildIds _ 0 = []
dataASTChildIds nextId i = dataASTChildIds nextId (i-1) ++ [nextId ++ "." ++ show i]

-- Add child nodes to AST
-- dataASTChildNodes :: [[Char]] -> [[Char]] -> [Statement]  -- TODO: change data type [[Char]]
-- dataASTChildNodes [] _ = []
-- dataASTChildNodes (id : ids) (arg : args) = createNode id arg : dataASTChildNodes ids args
-- take first id to create node for first field
-- TODO: add showConstr?