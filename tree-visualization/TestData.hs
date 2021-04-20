{-# LANGUAGE DeriveDataTypeable #-}

module TestData where

import Data.Typeable
import Data.Data

data Foo = Foo Char Int deriving (Data)

data Exp = Num      Int 
         | Plus     Exp Exp 
         | Times    Exp Exp 
         deriving (Data)

data Op = PlusOp | MinusOp | TimesOp | DivOp
        deriving (Data)

data Exp2 = Num2      Double
          | BinOp     Exp2 Op Exp2
          | ExpList   [Exp]
          | ExpLists  [Exp] [Exp2]
          deriving (Data)

data Stuff = None  
           | This Int 
           | That Bool
           | That2 (Stuff, Stuff)
           | That3 (Stuff, Stuff, Stuff)
           | Those [Stuff] 
           deriving (Data)

data Shape = Circle         Double Double Double 
           | Square         Double Double Double
           | RightTriangle  Double Double Double Double
           deriving (Data)

data IntList = Empty
             | Cons   Int IntList
             deriving (Data)

data StringBinaryTree = Leaf    String 
                      | Node    String StringBinaryTree StringBinaryTree
                      deriving (Data)

data Stack a = EmptyStack
             | MkStack      a (Stack a)
             deriving (Data)

-- data Foo = Foo Char Int deriving (Data, Typeable)

-- data Exp = Num      Int 
--          | Plus     Exp Exp 
--          | Times    Exp Exp 
--          deriving (Show, Data, Typeable)

-- data Op = PlusOp | MinusOp | TimesOp | DivOp
--         deriving (Show, Data, Typeable)

-- data Exp2 = Num2      Double
--           | BinOp     Exp2 Op Exp2
--           | ExpList   [Exp]
--           | ExpLists  [Exp] [Exp2]
--           deriving (Show, Data, Typeable)

-- data Stuff = None  
--            | This Int 
--            | That Bool
--            | That2 (Stuff, Stuff)
--            | That3 (Stuff, Stuff, Stuff)
--            | Those [Stuff] 
--            deriving (Show, Data, Typeable)

-- data Shape = Circle         Double Double Double 
--            | Square         Double Double Double
--            | RightTriangle  Double Double Double Double
--            deriving (Show, Data, Typeable)

-- data IntList = Empty
--              | Cons   Int IntList
--              deriving (Show, Data, Typeable)

-- data StringBinaryTree = Leaf    String 
--                       | Node    String StringBinaryTree StringBinaryTree
--                       deriving (Show, Data, Typeable)

-- data Stack a = EmptyStack
--              | MkStack      a (Stack a)
--              deriving (Show, Data, Typeable)