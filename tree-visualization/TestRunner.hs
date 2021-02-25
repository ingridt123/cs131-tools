module TestRunner where

import Data.Data
import System.Process
import AST
import GenericAST
import TestData

-- Tester function to create dot and png files of AST
run :: Data d => d -> String -> Int -> IO ProcessHandle
run d str num = createASTFiles ("genericAST-" ++ str ++ "-" ++ show num) (dataAST d)

-- Tester function to run all tests
runAllTests :: Int -> IO ProcessHandle
runAllTests num = do
    runFooTests num
    runExpTests num
    runExp2Tests num
    runStuffTests num
    runShapeTests num
    runIntListTests num
    -- runStrBinTreeTests num


----------------------
-- Foo
----------------------
foo_1 = Foo ')' 2930

runFooTests :: Int -> IO ProcessHandle 
runFooTests num = do
    run foo_1 "foo_1" num


----------------------
-- Exp
----------------------
exp_1 = Times (Num 10) (Times (Num 3) (Plus (Num 1) (Num 11)))
exp_2 = Times (Plus (Num 2) (Num 5)) (Num 8)

runExpTests :: Int -> IO ProcessHandle 
runExpTests num = do
    run exp_1 "exp_1" num
    run exp_2 "exp_2" num


----------------------
-- Exp2
----------------------
exp2_1 = BinOp (Num2 2.0) MinusOp (BinOp (BinOp (Num2 7.0) TimesOp (BinOp (Num2 2.0) PlusOp (Num2 3.0))) DivOp (BinOp (Num2 1.0) PlusOp (Num2 1.0)))
exp2_2 = ExpList [exp_1, exp_2]
exp2_3 = ExpLists [exp_1, exp_2, exp_1] [exp2_1, exp2_2]

runExp2Tests :: Int -> IO ProcessHandle 
runExp2Tests num = do
    run exp2_1 "exp2_1" num
    run exp2_2 "exp2_2" num
    run exp2_3 "exp2_3" num


----------------------
-- Stuff
----------------------
stuff_1 = This 5
stuff_2 = That False
stuff_3 = None
stuff_4 = Those [stuff_1,stuff_2,stuff_3]
stuff_5 = (stuff_1,stuff_2)
stuff_6 = That3 (stuff_1, stuff_2, stuff_4)

runStuffTests :: Int -> IO ProcessHandle 
runStuffTests num = do
    run stuff_1 "stuff_1" num
    run stuff_2 "stuff_2" num
    run stuff_3 "stuff_3" num
    run stuff_4 "stuff_4" num
    run stuff_5 "stuff_5" num
    run stuff_6 "stuff_6" num


----------------------
-- Shape
----------------------
shape_1 = Circle 5.0 2.0 3.0
shape_2 = RightTriangle 10.0 2.0 3.0 8.0

runShapeTests :: Int -> IO ProcessHandle 
runShapeTests num = do
    run shape_1 "shape_1" num
    run shape_2 "shape_2" num


----------------------
-- IntList
----------------------
intList_1 :: IntList
intList_1 = Cons 5 (Cons 12 (Cons 30 (Cons 1 Empty)))

runIntListTests :: Int -> IO ProcessHandle 
runIntListTests num = do
    run intList_1 "intList_1" num


----------------------
-- StringBinaryTree
----------------------
strBinTree_1 = Node "2" (Leaf "91") (Leaf "hello")
strBinTree_2 = Node "2" (Node "hello" (Leaf "91") (Node "j" (Leaf "") (Leaf "200"))) (Node "3" (Leaf "__") (Leaf "293"))
strBinTree_3 = Leaf "81"
strBinTree_4 = Node "2" (Node "hello" (Leaf "91") (Node "j" (Leaf "") (Leaf "200"))) (Node "3" (Leaf "my leaf") (Leaf "293"))
strBinTree_5 = Leaf "my leaf"

runStrBinTreeTests :: Int -> IO ProcessHandle 
runStrBinTreeTests num = do
    run strBinTree_1 "strBinTree_1" num
    run strBinTree_2 "strBinTree_2" num
    run strBinTree_3 "strBinTree_3" num
    run strBinTree_4 "strBinTree_4" num
    run strBinTree_5 "strBinTree_5" num


----------------------
-- Stack
----------------------
stack_1 :: Stack [Char]
stack_1 = EmptyStack
stack_2 :: Stack [Char]
stack_2 = MkStack "top" (MkStack "2nd" (MkStack "here you go" EmptyStack))
stack_3 :: Stack (Stack [Char])
stack_3 = MkStack stack_2 EmptyStack
stack_4 :: Stack Double
stack_4 = MkStack 293.01 (MkStack 8.0 (MkStack 15.20 (MkStack 88.90102 EmptyStack)))

runStackTests :: Int -> IO ProcessHandle 
runStackTests num = do
    run stack_1 "stack_1" num
    run stack_2 "stack_2" num
    run stack_3 "stack_3" num
    run stack_4 "stack_4" num



-- run = createDotFile "genericAST-test9" (dataAST (Times (Num 10) (Times (Num 3) (Plus (Num 1) (Num 11)))))
-- run = createDotFile "genericAST-test10" (dataAST (Times (Plus (Num 2) (Num 5)) (Num 8)))
-- run = createDotFile "genericAST-test11" (dataAST (BinOp (Num2 2.0) MinusOp (BinOp (BinOp (Num2 7.0) TimesOp (BinOp (Num2 2.0) PlusOp (Num2 3.0))) DivOp (BinOp (Num2 1.0) PlusOp (Num2 1.0)))))