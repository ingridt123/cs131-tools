# Breaking Down `gASTStatements` in `GenericAST`

The `gASTStatements` function constitutes the main logic of `GenericAST`, building a list of statements for the AST that is then used by `gAST` (and therefore `gStoreAST`) to create the DotGraph (in the Dot library).

```
-- | Difference list of statements for DotGraph
type StatementS = [Statement] -> [Statement]

-- | Build list of statements for AST
gASTStatements :: Data d => [Char]        -- ^ Id of next node in AST
                         -> d             -- ^ Generic data, must derive (Data)
                         -> Bool          -- ^ True to expand String as [Char] in AST, False otherwise
                         -> StatementS    -- ^ Difference list of statements for AST
```

The generic data `d` must derive `Data` to use functions such as `toConstr`.

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

`StatementS = [Statement] -> [Statement]` is used instead of concatenating multiple `[Statement]` because
***** TODO


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
-- | Build list of statements for AST
gASTStatements :: Data d => [Char]        -- ^ Id of next node in AST
                         -> d             -- ^ Generic data, must derive (Show, Data, Typeable)
                         -> Bool          -- ^ True to expand String as [Char] in AST, False otherwise
                         -> StatementS    -- ^ Difference list of statements for AST
gASTStatements nextId d expandStr = 
      if (typeOf d == typeOf "") && not expandStr
            then createNodeS nextId (gshow d)                           -- LINE 1
            else (createNodeS nextId . showConstr . toConstr $ d)       -- LINE 2
                  . (foldr (.) id . gASTEdges nextId $ childIds)        -- LINE 3
                  . (foldr (.) id . subtrees $ d)                       -- LINE 4
    where childNum   = glength d
          childIds   = gASTChildIds nextId childNum
          subtrees d = map (\i -> gmapQi i (gASTStatements (childIds !! i)) d expandStr) [0,1..(glength d - 1)]
```

The following sections break down the `gASTStatements` function line by line.

#### Line 1: String Leaf Node
```
if (typeOf d == typeOf "") && not expandStr
    then createNodeS nextId (gshow d)
```
If `d` is a `String` and `expandStr = False`, then one node is created for `d` where the label is the string `d`. This is a separate case since no node is created for the constructor, hence no edges need to be created from the constructor node to the subterm node(s). Additionally, instead of recursing into the `[Char]` to create subtrees of nodes, only a single node is created. Note that any terms that are Strings must be leaf nodes since `String`s don't have any subterms.

The decision was made not to create a constructor node in this situation, since the constructor of non-empty `String`s are `(:)` and `[]` for `""` (as `String = [Char]`), but this is not informative if the tree does not expand `String`s as `[Char]`s.

#### Line 2: Constructor
```
(createNodeS nextId . showConstr . toConstr $ d) :: StatementS
```
The second line creates a difference list representing a 1-element list containing a node for the name of the constructor with id `nextId`. For example,
```
d :: Exp2
d = Num2 2.0

showConstr . toConstr $ d :: String
    = "Num2"

-- Difference list representing node "Num2" with id "1.1"
-- In Dot language: "1.1" [label="Num2"]
createNodeS "1.1" . showConstr . toConstr $ d :: [StatementS]
```

#### Line 3: Parent -> Child Edges
```
(foldr (.) id . gASTEdges nextId $ childIds) :: StatementS
```
The third line creates a difference list representing a list containing edges from the parent node (with id `nextId`) to each of the child nodes. Each element in `childIds` is an id for these child nodes.

Note that if `d` is a `String` and `expandStr = False` (i.e. `String`s should not be expanded), then there will only be one child node and thus only one element in `childIds`. Otherwise, the number of child nodes is equal to the number of immediate subterms of `d`. For example, `BinOp (BinOp (Num2 5.0) PlusOp (BinOp (Num2 2.0) TimesOp (Num2 6.0))) MinusOp (Num2 4.0)` has 3 immediate subterms `BinOp (Num2 5.0) ...`, `MinusOp` and `(Num2 4.0)`.

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

#### Line 4: Recursion
```
(foldr (.) id . subtrees $ d) :: StatementS

subtrees d :: [StatementS]
where subtrees d = if strNode
                   then [createNodeS (head childIds) (gshow d)]
                   else map (\i -> gmapQi i (gASTStatements (childIds !! i)) d expandStr) [0,1..(glength d - 1)]
```
The last line recursively applies `gASTStatements` to each of the immediate subterm(s) of `d`. This is done using the `map` function over the list `[0,1..(glength d - 1)]` (where `glength d` is the number of immediate subterms of `d`), and `gmapQi` applies `(gASTStatements (childIds !! i))` to the i'th immediate subterm of `d`.

Similar to the previous line, since `subtrees` returns [StatementS], we again use `foldr (.) id` to combine these `StatementS` lists into `StatementS` (see section above for explanation).

#### Putting It All Together

These three lines, all of which return `StatementS`, are combined using the composition operation. Therefore, when `[]` is applied to `gASTStatements` in `gAST`, the list of statements represented by `StatementS` returned by `gASTStatements` is prepended to `[]` and a list of `Statement`s are returned, which are then used to create the `DotGraph`.