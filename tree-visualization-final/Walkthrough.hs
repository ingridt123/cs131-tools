{-# LANGUAGE DeriveDataTypeable #-}

module Walkthrough where

import Data.Data (Data)
import GenericAST

data Op = PlusOp | MinusOp | TimesOp | DivOp
        deriving (Data)

data Exp2 = Num2    Double 
          | BinOp   Exp2 Op Exp2
          deriving (Data)

-- 5 + 2 * 6 - 4
exp2_1 :: Exp2
exp2_1 = BinOp (BinOp (Num2 5.0) PlusOp (BinOp (Num2 2.0) TimesOp (Num2 6.0))) MinusOp (Num2 4.0)

dotFiles_exp2_1 = gStoreAST exp2_1 False "sample-output" "exp2_1_sample"

-- 2.2 - ((7.4 * (2.82 + 3.1)) / (1.3 + 1.999))
exp2_2 :: Exp2
exp2_2 = BinOp (Num2 2.2) MinusOp (BinOp (BinOp (Num2 7.4) TimesOp (BinOp (Num2 2.82) PlusOp (Num2 3.1))) DivOp (BinOp (Num2 1.3) PlusOp (Num2 1.999)))

dotFiles_exp2_2 = gStoreAST exp2_2 False "sample-output" "exp2_2_sample"
































{-# ANN module "HLint: ignore Use camelCase" #-}