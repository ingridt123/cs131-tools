# Introducing `GenericAST`

Note: A walkthrough video is available [here](https://www.youtube.com/watch?v=aFQ-gwV8mEM). See `Walkthrough.hs`, `StringBinTree.hs` (both from the video) `Sample.hs` for code samples.

Say we want to encode the following arithmetic equation in Haskell:
```
5 + 2 * 6 - 4
```

We can define a data type `Exp2` as follows where `Num2` represents each term in the equation and `BinOp` represents two terms combined by an arithmetic operation.
```
data Exp2 = Num2      Double
          | BinOp     Exp2 Op Exp2

data Op = PlusOp | MinusOp | TimesOp | DivOp
```

We can then define our data instance to be:
```
exp2_1 :: Exp2
exp2_1 = BinOp (BinOp (Num2 5.0) PlusOp (BinOp (Num2 2.0) TimesOp (Num2 6.0))) MinusOp (Num2 4.0)
```

At this point we have four terms and it's still fairly manageable. We can start from the inside, do the multiplication between 2.0 and 6.0, then add that to 5.0, and lastly subtract 4.0 from the sum.

But now what if we want to encode something more complicated, like this arithmetic equation:
```
2.2 - ((7.4 * (2.82 + 3.1)) / (1.3 + 1.999))
```

Using the data type `Exp2` we defined above, in Haskell this would be encoded as:
```
exp2_2 :: Exp2
exp2_2 = BinOp (Num2 2.2) MinusOp (BinOp (BinOp (Num2 7.4) TimesOp (BinOp (Num2 2.82) PlusOp (Num2 3.1))) DivOp (BinOp (Num2 1.3) PlusOp (Num2 1.999)))
```
With 6 terms, this already becomes much more difficult to reason about.

This is where `GenericAST` can come in handy. All you need to do is have your data type derive the `Data` typeclass and you can use `GenericAST` to visualize your data in an abstract syntax tree (AST)!

So let's add the typeclass to our data type:
```
{-# LANGUAGE DeriveDataTypeable #-}
import Data.Data ( Data )

data Exp2 = Num2      Doble
          | BinOp     Exp2 Op Exp2
          deriving (Data)

data Op = PlusOp | MinusOp | TimesOp | DivOp
        deriving (Data)
```

And don't forget to import `GenericAST` as well.
```
import GenericAST
```

Now all we need to do is use `GenericAST` to create our AST and store both the dot and png files to the sample-output directory!
```
dotFiles_exp2_1 = gStoreAST exp2_1 False "sample-output" "exp2_1_sample"
```
![AST for `exp2_1`](sample-output/exp2_1_sample.png)

```
dotFiles_exp2_2 = gStoreAST exp2_2 False "sample-output" "exp2_2_sample"
```
![AST for `exp2_2`](sample-output/exp2_2_sample.png)

And here's the breakdown of the parameters of `gStoreAST`:
```
-- Store AST dot and png files of dot graph in dirName as fileName
gStoreAST :: Data d => d                  -- ^ Data instance, must derive (Data)
                    -> Bool               -- ^ True to expand String as [Char] in AST, False otherwise
                    -> FilePath           -- ^ Name of directory, set to "" to store in current directory
                    -> FilePath           -- ^ Base name of file
                    -> IO ProcessHandle
```

Below is an example where we set `expandStr` to `True`
```
data StringBinaryTree = Leaf    String 
                      | Node    String StringBinaryTree StringBinaryTree
                      deriving (Data)
                      
strBinTree = Node "2" (Node "hello" (Leaf "91") (Node "j" (Leaf "") (Leaf "200"))) (Node "3" (Leaf "__") (Leaf "293"))
```

```
dotFiles_strBinTree_false = gStoreAST strBinTree False "sample-output" "strBinTree_false_sample"
```
![AST for `strBinTree (false)`](sample-output/strBinTree_false_sample.png)

```
dotFiles_strBinTree_true = gStoreAST strBinTree True "sample-output" "strBinTree_true_sample"
```
![AST for `strBinTree (true)`](sample-output/strBinTree_true_sample.png)
