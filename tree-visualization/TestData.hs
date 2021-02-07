module TestData where

data Exp = Num      Int 
         | Plus     Exp Exp 
         | Times    Exp Exp 
           deriving Show

data Op = PlusOp | MinusOp | TimesOp | DivOp
        deriving Show

data Exp2 = Num2      Double
          | BinOp     Exp2 Op Exp2
            deriving Show