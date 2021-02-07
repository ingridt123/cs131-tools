module TestAST where

import AST
import Dot
import TestData


run :: IO()
-- run = createDotFile "expAST" (expAST (Times (Plus (Num 2) (Num 5)) (Num 8)))
-- run = createDotFile "expAST2" (expAST (Plus (Num 10) (Times (Num 3) (Plus (Num 1) (Num 11)))))
run = createDotFile "expAST3" (expAST (Times (Num 10) (Times (Num 3) (Plus (Num 1) (Num 11)))))

rootId = "1"

-- | Exp tree
expAST :: Exp -> DotGraph
expAST e = DotGraph NonStrict Directed (Just (convertToId "")) (expASTStatements rootId e)

expASTStatements :: [Char] -> Exp -> [Statement]
expASTStatements nextId (Num n) = [ 
        createNode nextId "Num",
        createNode childId (show n),
        createEdge nextId childId
    ]
    where childId = nextId ++ "." ++ show 1
expASTStatements nextId e = [
        createNode nextId nodeText,
        createEdge nextId leftId,
        createEdge nextId rightId
    ] ++ expASTStatements leftId e1 ++ expASTStatements rightId e2
    where (nodeText, e1, e2) = expASTNodeInfo e
          leftId = nextId ++ "." ++ show 1
          rightId = nextId ++ "." ++ show 2

expASTNodeInfo :: Exp -> ([Char], Exp, Exp)
expASTNodeInfo (Plus e1 e2) = ("Plus", e1, e2)
expASTNodeInfo (Times e1 e2) = ("Times", e1, e2)


-- | Exp2 Tree
exp2AST :: Exp2 -> DotGraph
exp2AST e = DotGraph NonStrict Directed (Just (convertToId "")) (exp2ASTStatements rootId e)

exp2ASTStatements :: [Char] -> Exp2 -> [Statement]
exp2ASTStatements nextId (Num2 n) = [ 
        createNode nextId "Num",                        -- node for name of data type (parent)
        createNode childId (show n),                    -- node for value of data type (child)
        createEdge nextId childId                       -- edge from parent to child
    ]
    where childId = nextId ++ "." ++ show 1
exp2ASTStatements nextId (BinOp e1 op e2) = [
        createNode nextId "BinOp",                      -- node for name of data type (parent)
        createEdge nextId leftId,                       -- edge from parent to child1
        createEdge nextId midId,                        -- edge from parent to child2
        createEdge nextId rightId                       -- edge from parent to child3
    ] ++ exp2ASTStatements leftId e1                    -- subtree for value 1 of data type (child1)
      ++ [createNode midId (show op)]                   -- node for value 2 of data type (child2)
      ++ exp2ASTStatements rightId e2                   -- subtree for value 3 of data type (child3)
    where leftId = nextId ++ "." ++ show 1              
          midId = nextId ++ "." ++ show 2               -- increment count for ids from 1 --> # of children
          rightId = nextId ++ "." ++ show 3

run2 = createDotFile "exp2AST1" (exp2AST (BinOp (Num2 2.0) MinusOp (BinOp (BinOp (Num2 7.0) TimesOp (BinOp (Num2 2.0) PlusOp (Num2 3.0))) DivOp (BinOp (Num2 1.0) PlusOp (Num2 1.0)))))