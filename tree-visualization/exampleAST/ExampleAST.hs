module ExampleAST (main) where

import Dot
import Data.Text as T

-- | Draws AST from Exp data type
-- drawAST :: Exp -> 

main :: IO ()
main = do
  putStrLn $ "dumping example dotgraph to " ++ target
  encodeToFile target example
 
target :: FilePath
target = "example.dot"

example :: DotGraph
example = DotGraph NonStrict Directed (Just (Id (T.pack ""))) 
          [ StatementNode $ NodeStatement (NodeId (Id (T.pack "0")) Nothing)
            [ Attribute (Id (T.pack "label")) (Id (T.pack "Plus"))
            -- , Attribute (Id (T.pack "shape")) (Id (T.pack "box"))
            ]
          , StatementNode $ NodeStatement (NodeId (Id (T.pack "1")) Nothing) 
            [
              Attribute (Id (T.pack "label")) (Id (T.pack "Num"))
            ]
          , StatementEdge $ EdgeStatement (ListTwo (EdgeNode (NodeId (Id (T.pack "0")) Nothing)) 
                                                   (EdgeNode (NodeId (Id (T.pack "1")) Nothing)) 
                                                  [])
            [ 
              -- Attribute (Id (T.pack "color")) (Id (T.pack "red"))
            ]
          ]