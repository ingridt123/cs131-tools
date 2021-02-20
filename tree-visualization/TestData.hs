{-# LANGUAGE DeriveDataTypeable #-}

module TestData where

import Data.Typeable
import Data.Data 

data Foo = Foo Char Int deriving (Data,Typeable)

data Exp = Num      Int 
         | Plus     Exp Exp 
         | Times    Exp Exp 
           deriving (Show, Data, Typeable)

data Op = PlusOp | MinusOp | TimesOp | DivOp
        deriving (Show, Data, Typeable)

data Exp2 = Num2      Double
          | BinOp     Exp2 Op Exp2
            deriving (Show, Data, Typeable)

data Stuff
    = None  
    | This Int 
    | That Bool
    | That2 (Stuff, Stuff)
    | Those [Stuff] 
    deriving (Show, Data, Typeable)