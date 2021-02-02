module AST where

import Dot
import Data.Text as T


createDotFile :: [Char] -> DotGraph -> IO ()
createDotFile fileName dotGraph = do
  putStrLn $ "dumping example dotgraph to " ++ target fileName
  encodeToFile (target fileName) dotGraph


target :: [Char] -> FilePath
target fileName = fileName ++ ".dot"

convertToId :: [Char] -> Id
convertToId id = Id (T.pack id)

convertToNodeId :: [Char] -> NodeId 
convertToNodeId id = NodeId (convertToId id) Nothing

createNode :: [Char] -> [Char] -> Statement
createNode id label = 
    StatementNode $ NodeStatement (convertToNodeId id) 
                                  [Attribute (convertToId "label") (convertToId label)]

createEdge :: [Char] -> [Char] -> Statement 
createEdge id1 id2 =
    StatementEdge $ EdgeStatement (ListTwo (EdgeNode (convertToNodeId id1)) 
                                           (EdgeNode (convertToNodeId id2)) 
                                           [])
                                  []


emptyGraph :: DotGraph
emptyGraph = DotGraph NonStrict Directed (Just (convertToId "")) []