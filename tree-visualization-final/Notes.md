# Breaking Down `gASTStatements` in `GenericAST`

The `gASTStatements` function constitutes the main logic of `GenericAST`, building a list of statements for the AST that is then used by `gAST` (and therefore `gStoreAST`) to create the DotGraph (in the Dot library).

```
-- | Difference list of statements for DotGraph
type StatementS = [Statement] -> [Statement]

-- | Build list of statements for AST
gASTStatements :: Data d => [Char]        -- ^ Id of next node in AST
                         -> d             -- ^ Generic data, must derive (Show, Data, Typeable)
                         -> Bool          -- ^ True to expand String as [Char] in AST, False otherwise
                         -> StatementS    -- ^ Difference list of statements for AST
```

### `StatementS`
The data type `StatementS` applies similar logic to `ShowS` in that it is also a [difference list](https://wiki.haskell.org/Difference_list). Thus when function `f :: StatementS = [Statement] -> [Statement]` (representing the list `xs :: [Statement]`) is given another list `ys :: [Statement]`, it returns the list that `f` represents prepended to `ys`. In other words,
```
f ys = xs ++ ys
```
This means that we can compose multiple `StatementS` functions to concatenate multiple `Statement` lists. For example, suppose `f1 :: StatementS` represents `xs1 :: [Statement]`, `f2 :: StatementS` represents `xs2 :: [Statement]`, ..., `fn :: StatementS` represents `xsn :: [Statement]`, then
```
f1 . f2 . ... . fn :: StatementS
    = (++) (xs1 ++ xs2 ++ ... ++ xsn)

f1 . f2 . ... . fn []
    = (++) (xs1 ++ xs2 ++ ... ++ xsn) [] :: [Statement]
    = xs1 ++ xs2 ++ ... ++ xsn ++ []
```

### `gASTEdges` and `gASTChildIds`
The `gASTStatements` function uses functions `gASTEdges` and `gASTChildIds` defined specifically for building ASTs in `GenericAST`:
```
-- | Build list of edge statements for AST from parentId to ids
gASTEdges :: String           -- ^ Id of parent node
          -> [String]         -- ^ List of ids of child nodes
          -> [StatementS]     -- ^ Difference list of edge statements

-- | Build list of child ids for node with parentId in AST
gASTChildIds :: String        -- ^ Id of parent node
             -> Int           -- ^ Number of child nodes remaining
             -> [String]      -- ^ List of child ids
```
For example, `gASTChildIds "1.1" 4` returns `["1.1.1", "1.1.2", "1.1.3", "1.1.4"]`.

Additionally, the following functions are defined in `GenericAST` for interfacing with the Dot library:
```
-- Create node statement, represented by StatementNode in Dot library
createNode :: String -> String -> Statement

-- | Version of createNode that returns StatementS instead of Statement
createNodeS :: String -> String -> StatementS

-- Create edge statement, represented by StatementEdge in Dot library
createEdge :: String -> String -> Statement 

-- | Version of createEdge that return StatementS instead of Statement
createEdgeS :: String -> String -> StatementS
```

### `gASTStatements`
This is the full code for the `gASTStatements` function:
```
gASTStatements nextId d expandStr = 
      (createNodeS nextId . showConstr . toConstr $ d)
    . (foldr (.) id . gASTEdges nextId $ childIds)
    . (foldr (.) id . subtrees $ d)
    where isString d = (typeOf d == typeOf "") && not expandStr
          childNum   = if isString d then 1 else glength d
          childIds   = gASTChildIds nextId childNum
          subtrees d = if isString d
                then [createNodeS (head childIds) (gshow d)]
                else map (\i -> gmapQi i (gASTStatements (childIds !! i)) d expandStr) [0,1..(glength d - 1)]
```

Breaking down the `gASTStatements` function line by line:

#### Line 1: Constructor
```
(createNodeS nextId . showConstr . toConstr $ d) :: StatementS
```
This first line creates a difference list representing a 1-element list containing a node for the name of the constructor with id `nextId`. For example,
```
d :: Exp2
d = Num2 2.0

showConstr . toConstr $ d :: String
    = "Num2"

-- Difference list representing node "Num2" with id "1.1"
-- In Dot language: "1.1" [label="Num2"]
createNodeS "1.1" . showConstr . toConstr $ d :: [StatementS]
```

#### Line 2: Parent -> Child Edges
This second line creates a difference list representing a list containing edges from the parent node (with id `nextId`) to each of the child nodes. Each element in `childIds` is an id for these child nodes.
```
(foldr (.) id . gASTEdges nextId $ childIds) :: StatementS
```

Since the return type of `gASTEdges` is `[StatementS]`, we will use `foldr (.) id` to combine these `StatementS` lists into `StatementS`. Suppose that `f1 :: StatementS` represents `xs1 :: [Statement]`, `f2 :: StatementS` represents `xs2 :: [Statement]`, ..., `fn :: StatementS` represents `xsn :: [Statement]`, then it follows that:
```
foldr (.) id :: Foldable t => t (b -> b) -> b -> b
foldr (.) id [f1, f2, ..., fn] :: StatementS
    = f1 . (f2 . ... (fn . id) ...)
```
We know that `StatementS = [Statement] -> [Statement]` is the type since if we apply `[]` to the function, the function returns `[Statement]`, as shown below:
```
foldr (.) id [f1, f2, ..., fn] [] :: [Statement]
    = f1 . (f2 . ... (fn . id []) ...)
    = f1 . (f2 . ... (fn []) ...)
    = f1 . (f2 . ... (xsn []) ...)
    = ...
    = f1 (xs2 ++ ... ++ xsn)
    = xs1 ++ xs2 ++ ... ++ xsn
```




- why three typeclasses needed