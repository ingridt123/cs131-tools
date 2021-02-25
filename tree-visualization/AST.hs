module AST where

import Dot
import Data.Text as T
import System.Process

-- TODO: look at encodeCompass / CardinalDirection in dot hackage
-- TODO: fix (" " ++ label ++ " ") otherwise Node not "Node" in dot file

createASTFiles :: [Char] -> DotGraph -> IO ProcessHandle
createASTFiles fileName dotGraph = do
  putStrLn $ "Dumping dotgraph to " ++ targetDot fileName
  putStrLn $ "Dumping png to " ++ targetPng fileName
  encodeToFile (targetDot fileName) dotGraph
  runCommand ("dot " ++ targetDot fileName ++ " -Tpng > " ++ targetPng fileName)


targetDot :: String -> FilePath
targetDot fileName = fileName ++ ".dot"

targetPng :: String -> FilePath
targetPng fileName = fileName ++ ".png"

convertToId :: String -> Id
convertToId id = Id (T.pack id)

convertToNodeId :: String -> NodeId 
convertToNodeId id = NodeId (convertToId id) Nothing

createNode :: String -> String -> Statement
createNode id label = 
    StatementNode $ NodeStatement (convertToNodeId id) 
                                  [Attribute (convertToId "label") (convertToId (" " ++ label ++ " "))]

createEdge :: String -> String -> Statement 
createEdge id1 id2 =
    StatementEdge $ EdgeStatement (ListTwo (EdgeNode (convertToNodeId id1)) 
                                           (EdgeNode (convertToNodeId id2)) 
                                           [])
                                  []


emptyGraph :: DotGraph
emptyGraph = DotGraph NonStrict Directed (Just (convertToId "")) []