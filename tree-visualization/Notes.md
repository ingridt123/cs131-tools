# Generalizing Tree Visualization

* `TestData.hs` contains data types being used for testing
* `GenericAST.hs` contains code for generating ASTs for any data type
* `AST.hs` contains helper functions for generating AST and creating DOT files (using the Dot Hackage)
* `TestAST.hs` contains code for generating ASTs for Exp and Exp2


### Defining the Data Type
```
data Data = Parent1     Child1.1 Child1.2 ... Child1.n_1
          | Parent2     Child2.1 Child2.2 ... Child2.n_2
          | ...
          | Parentk     Childk.1 Childk.2 ... Childk.n_k
          deriving (Show, Data, Typeable)
```

* Can have recursive data types (e.g. `Child1.2` has similar structure as Data)



### Building the AST
```
name n = Just (convertToId n)
rootId = "1"

dataAST :: Data -> DotGraph
dataAST d = DotGraph NonStrict Directed (name "") (dataASTStatements rootId d)

dataASTStatements :: [Char] -> Data -> [Statement]
dataASTStatements nextId ? = [?]
```

* `createNode` for "Parentj"

  ```
  createNode nextId showConstr (toConstr ...)
  ```

* `createEdge` from parent node to all child nodes Childj.1 ... Childj.n_j (nextId to newIds) <-- j = number of fields in constructor

* `createNode` for all child nodes Childj.1 ... Childj.n_j
    * For nodes Childj.x that are the value itself (e.g. `Double`, `Op = PlusOp | MinusOp | TimesOp | DivOp`), create a node for the value (e.g. "2.0", "TimesOp") using `show`

      ```
      createNode childId showConstr (toConstr ...)
      ```

    * For nodes Childj.y that are recursive data structures (e.g. `Exp`, `Exp2`), recursively call `dataASTStatements` with a new id (`nextId ++ "." ++ show y`) and Childj.y -- get each constructor field



### Saving to DOT File
```
createDotFile :: [Char] -> DotGraph -> IO ()
createDotFile "dataAST" (dataAST (Data ...))
```


### Converting DOT File to PNG
```
dot files.dot -Tpng > files.png
```


### Notes
```
> show (toConstr (Plus (Num 1) (Num 2)))
"Plus"

> dataTypeOf ((Plus (Num 1) (Num 2)))
DataType {tycon = "Exp", datarep = AlgRep [Num,Plus,Times]}

> gmapQ (\d -> typeOf d) ((BinOp (Num2 1.0) TimesOp 
> gmapQ typeOf ((BinOp (Num2 1.0) TimesOp (BinOp (Num2 5.0) PlusOp (Num2 8.0))))
[Exp2,Op,Exp2]

> gmapQ (\d -> toConstr d) ((BinOp (Num2 1.0) TimesOp (BinOp (Num2 5.0) PlusOp (Num2 8.0))))
[Num2,TimesOp,BinOp]

> gmapQ (\d -> toConstr d) (Num2 2.0)
[2.0]

> gmapQ (\d -> toConstr d) TimesOp
[]

> gmapQ (\d -> dataTypeOf d) ((BinOp (Num2 1.0) TimesOp (BinOp (Num2 5.0) PlusOp (Num2 8.0))))
[DataType {tycon = "Exp2", datarep = AlgRep [Num2,BinOp]},DataType {tycon = "Op", datarep = AlgRep [PlusOp,MinusOp,TimesOp,DivOp]},DataType {tycon = "Exp2", datarep = AlgRep [Num2,BinOp]}]

> gmapQ gshow (Times (Num 10) (Times (Num 3) (Plus (Num 1) (Num 11))))
["(Num (10))","(Times (Num (3)) (Plus (Num (1)) (Num (11))))"]

> gmapQ gshow (BinOp (Num2 2.0) MinusOp (BinOp (BinOp (Num2 7.0) TimesOp (BinOp (Num2 2.0) PlusOp (Num2 3.0))) DivOp (BinOp (Num2 1.0) PlusOp (Num2 1.0))))
["(Num2 (2.0))","(MinusOp)","(BinOp (BinOp (Num2 (7.0)) (TimesOp) (BinOp (Num2 (2.0)) (PlusOp) (Num2 (3.0)))) (DivOp) (BinOp (Num2 (1.0)) (PlusOp) (Num2 (1.0))))"]
```