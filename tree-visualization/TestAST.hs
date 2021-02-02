module TestAST where

import AST
import Dot
import TestData


run :: IO()
-- run = createDotFile "expAST" (expAST (Times (Plus (Num 2) (Num 5)) (Num 8)))
-- run = createDotFile "expAST2" (expAST (Plus (Num 10) (Times (Num 3) (Plus (Num 1) (Num 11)))))
run = createDotFile "expAST3" (expAST (Times (Num 10) (Times (Num 3) (Plus (Num 1) (Num 11)))))

rootId = "1"

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