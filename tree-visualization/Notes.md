# Generalizing Tree Visualization

### Defining the Data Type
```
data Data = Parent1     Child1.1 Child1.2 ... Child1.n_1
          | Parent2     Child2.1 Child2.2 ... Child2.n_2
          | ...
          | Parentk     Childk.1 Childk.2 ... Childk.n_k
            deriving Show
```

* Can have recursive data types (e.g. `Child1.2` has similar structure as Data)

### Building the AST
```
name n = Just (converToId n)
rootId = "1"

dataAST :: Data -> DotGraph
dataAST d = DotGraph NonStrict Directed (name "") (dataASTStatements rootId d)

dataASTStatements :: [Char] -> Data -> [Statement]
dataASTStatements nextId ? = [?]
```

* `createNode` for "Parentj"
* `createEdge` from parent node to all child nodes Childj.1 ... Childj.n_j
* `createNode` for all child nodes Childj.1 ... Childj.n_j
    * For nodes Childj.x that are the value itself (e.g. `Double`, `Op = PlusOp | MinusOp | TimesOp | DivOp`), create a node for the value (e.g. "2.0", "TimesOp") using `show`
    * For nodes Childj.y that are recursive data structures (e.g. `Exp`, `Exp2`), recursively call `dataASTStatements` with a new id (`nextId ++ "." ++ show y`) and Childj.y

### Saving to DOT File
```
createDotFile :: [Char] -> DotGraph -> IO ()
createDotFile "dataAST" (dataAST (Data ...))
```