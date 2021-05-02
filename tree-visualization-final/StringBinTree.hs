{-# LANGUAGE DeriveDataTypeable #-}

module StringBinTree where

import Data.Data ( Data )
import GenericAST

data StringBinaryTree = Leaf    String 
                      | Node    String StringBinaryTree StringBinaryTree
                      deriving (Data)
                      
strBinTree :: StringBinaryTree
strBinTree = Node "2" (Node "hello" (Leaf "91") (Node "j" (Leaf "") (Leaf "200"))) (Node "3" (Leaf "__") (Leaf "293"))

dotFiles_strBinTree_false = gStoreAST strBinTree False "sample-output" "strBinTree_false_sample"
dotFiles_strBinTree_true = gStoreAST strBinTree True "sample-output" "strBinTree_true_sample"


















{-# ANN module "HLint: ignore Use camelCase" #-}