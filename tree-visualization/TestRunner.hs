module TestRunner where

import Data.Data
import System.Process
import AST
import GenericAST1
import GenericAST2
import TestData

-- | Directory name of test files
testDir :: [Char]
testDir = "genericAST-tests"

-- | Tester function to create dot and png files of AST
-- data:      instance of data type to build tree
-- name:      name of instance d (appended to end of file name)
-- runNum:    test run runNum moduleNumber
-- moduleNum: 1 for GenericAST1, 2 for GenericAST2
-- E.g. run foo_1 "foo_1" 0
-- TODO: add option to run with GenericAST1 or 2
run :: Data d => d -> String -> Int -> Int -> IO ProcessHandle
run d name runNum moduleNum = createASTFiles dirName fileName tree
    where dirName = "genericAST-" ++ show runNum
          fileName = "genericAST-" ++ name ++ "-" ++ show runNum
          tree = if moduleNum == 1 
                    then GenericAST1.dataAST d
                    else GenericAST2.dataAST d

-- Run all tests
runAllTests :: Int -> Int -> IO ProcessHandle
runAllTests runNum moduleNum = do
    runFooTests runNum moduleNum
    runExpTests runNum moduleNum
    runExp2Tests runNum moduleNum
    runStuffTests runNum moduleNum
    runShapeTests runNum moduleNum
    runIntListTests runNum moduleNum
    runStrBinTreeTests runNum moduleNum

-- | TODO 
-- verifyDotFiles :: FilePath -> Bool
-- verifyDotFiles dirName = verifyAllDotFiles
-- listDirectory

-- | Compare two dot files
-- Source: https://stackoverflow.com/questions/37352953/how-to-check-that-two-files-are-equal-in-haskell
cmpDotFiles :: FilePath -> FilePath -> IO Bool
cmpDotFiles a b = do
    aContents <- readFile a
    bContents <- readFile b
    return (aContents == bContents)

----------------------
-- Foo
----------------------
foo_1 = Foo ')' 2930

runFooTests :: Int -> Int -> IO ProcessHandle 
runFooTests runNum moduleNum = do
    run foo_1 "foo_1" runNum moduleNum


----------------------
-- Exp
----------------------
exp_1 = Times (Num 10) (Times (Num 3) (Plus (Num 1) (Num 11)))
exp_2 = Times (Plus (Num 2) (Num 5)) (Num 8)

runExpTests :: Int -> Int -> IO ProcessHandle 
runExpTests runNum moduleNum = do
    run exp_1 "exp_1" runNum moduleNum
    run exp_2 "exp_2" runNum moduleNum


----------------------
-- Exp2
----------------------
exp2_1 = BinOp (Num2 2.0) MinusOp (BinOp (BinOp (Num2 7.0) TimesOp (BinOp (Num2 2.0) PlusOp (Num2 3.0))) DivOp (BinOp (Num2 1.0) PlusOp (Num2 1.0)))
exp2_2 = ExpList [exp_1, exp_2]
exp2_3 = ExpLists [exp_1, exp_2, exp_1] [exp2_1, exp2_2]

runExp2Tests :: Int -> Int -> IO ProcessHandle 
runExp2Tests runNum moduleNum = do
    run exp2_1 "exp2_1" runNum moduleNum
    run exp2_2 "exp2_2" runNum moduleNum
    run exp2_3 "exp2_3" runNum moduleNum


----------------------
-- Stuff
----------------------
stuff_1 = This 5
stuff_2 = That False
stuff_3 = None
stuff_4 = Those [stuff_1,stuff_2,stuff_3]
stuff_5 = (stuff_1,stuff_2)
stuff_6 = That3 (stuff_1, stuff_2, stuff_4)

runStuffTests :: Int -> Int -> IO ProcessHandle 
runStuffTests runNum moduleNum = do
    run stuff_1 "stuff_1" runNum moduleNum
    run stuff_2 "stuff_2" runNum moduleNum
    run stuff_3 "stuff_3" runNum moduleNum
    run stuff_4 "stuff_4" runNum moduleNum
    run stuff_5 "stuff_5" runNum moduleNum
    run stuff_6 "stuff_6" runNum moduleNum


----------------------
-- Shape
----------------------
shape_1 = Circle 5.0 2.0 3.0
shape_2 = RightTriangle 10.0 2.0 3.0 8.0

runShapeTests :: Int -> Int -> IO ProcessHandle 
runShapeTests runNum moduleNum = do
    run shape_1 "shape_1" runNum moduleNum
    run shape_2 "shape_2" runNum moduleNum


----------------------
-- IntList
----------------------
intList_1 :: IntList
intList_1 = Cons 5 (Cons 12 (Cons 30 (Cons 1 Empty)))

runIntListTests :: Int -> Int -> IO ProcessHandle 
runIntListTests runNum moduleNum = do
    run intList_1 "intList_1" runNum moduleNum


----------------------
-- StringBinaryTree
----------------------
strBinTree_1 = Node "2" (Leaf "91") (Leaf "hello")
strBinTree_2 = Node "2" (Node "hello" (Leaf "91") (Node "j" (Leaf "") (Leaf "200"))) (Node "3" (Leaf "__") (Leaf "293"))
strBinTree_3 = Leaf "81"
strBinTree_4 = Node "2" (Node "hello" (Leaf "91") (Node "j" (Leaf "") (Leaf "200"))) (Node "3" (Leaf "my leaf") (Leaf "293"))
strBinTree_5 = Leaf "my leaf"

runStrBinTreeTests :: Int -> Int -> IO ProcessHandle 
runStrBinTreeTests runNum moduleNum = do
    run strBinTree_1 "strBinTree_1" runNum moduleNum
    run strBinTree_2 "strBinTree_2" runNum moduleNum
    run strBinTree_3 "strBinTree_3" runNum moduleNum
    run strBinTree_4 "strBinTree_4" runNum moduleNum
    run strBinTree_5 "strBinTree_5" runNum moduleNum


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
stack_5 :: Stack [Char]
stack_5 = MkStack ['a', 'b', 'c'] EmptyStack

runStackTests :: Int -> Int -> IO ProcessHandle 
runStackTests runNum moduleNum = do
    run stack_1 "stack_1" runNum moduleNum
    run stack_2 "stack_2" runNum moduleNum
    run stack_3 "stack_3" runNum moduleNum
    run stack_4 "stack_4" runNum moduleNum
    run stack_5 "stack_5" runNum moduleNum



-- run = createDotFile "genericAST-test9" (dataAST (Times (Num 10) (Times (Num 3) (Plus (Num 1) (Num 11)))))
-- run = createDotFile "genericAST-test10" (dataAST (Times (Plus (Num 2) (Num 5)) (Num 8)))
-- run = createDotFile "genericAST-test11" (dataAST (BinOp (Num2 2.0) MinusOp (BinOp (BinOp (Num2 7.0) TimesOp (BinOp (Num2 2.0) PlusOp (Num2 3.0))) DivOp (BinOp (Num2 1.0) PlusOp (Num2 1.0)))))