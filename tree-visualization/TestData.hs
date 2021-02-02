module TestData where

data Exp = Num      Int 
         | Plus     Exp Exp 
         | Times    Exp Exp 
           deriving Show

