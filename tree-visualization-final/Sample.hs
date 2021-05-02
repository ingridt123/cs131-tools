{-# LANGUAGE DeriveDataTypeable #-}

module Sample where

import Data.Data ( Data )
import GenericAST ( gAST, gStoreAST )

-- | Declare data type(s) deriving (Data)
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


-- | Declare instance(s) of data type(s)
exp_1 :: Exp
exp_1 = Times (Num 10) (Times (Num 3) (Plus (Num 1) (Num 11)))

exp_2 :: Exp
exp_2 = Times (Plus (Num 2) (Num 5)) (Num 8)

exp2_1 :: Exp2
exp2_1 = BinOp (BinOp (Num2 5.0) PlusOp (BinOp (Num2 2.0) TimesOp (Num2 6.0))) MinusOp (Num2 4.0)

exp2_2 :: Exp2
exp2_2 = BinOp (Num2 2.2) MinusOp (BinOp (BinOp (Num2 7.4) TimesOp (BinOp (Num2 2.82) PlusOp (Num2 3.1))) DivOp (BinOp (Num2 1.3) PlusOp (Num2 1.999)))

exp2_3 :: Exp2
exp2_3 = ExpList [exp_1, exp_2]

exp2_4 :: Exp2
exp2_4 = ExpLists [exp_1, exp_2, exp_1] [exp2_1, exp2_2]


-- | Call gAST to get DotGraph representation of AST
--   Call gStoreAST to store AST in dot and png format
dotGraph_exp_1 = gAST exp_1
dotGraph_exp2_1 = gAST exp2_1

-- Stores exp_2_sample.dot and exp_2_sample.png to current directory
dotFiles_exp_2 = gStoreAST exp_2 False "" "exp_2_sample"
-- Stores exp2_2_sample.dot and exp2_2_sample.png to sample directory
dotFiles_exp2_2 = gStoreAST exp2_2 False "sample" "exp2_2_sample"