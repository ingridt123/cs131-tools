module AST where

import Dot
import Data.Text as T
import Control.Concurrent
import System.Process
import System.Directory

-- TODO: look at encodeCompass / CardinalDirection in dot hackage
-- TODO: fix (" " ++ label ++ " ") otherwise Node not "Node" in dot file

-- dirName = "" to create file in root directory
createASTFiles :: String -> String -> DotGraph -> IO ProcessHandle
createASTFiles dirName fileName dotGraph = do
  createDirectoryIfMissing True dirName
  putStrLn $ "Dumping dotgraph to " ++ dotFile
  putStrLn $ "Dumping png to " ++ pngFile
  encodeToFile dotFile dotGraph
  runCommand ("dot " ++ dotFile ++ " -Tpng > " ++ pngFile)
  where dotFile = targetDot dirName fileName
        pngFile = targetPng dirName fileName


targetDot :: String -> String -> FilePath
targetDot "" fileName = fileName ++ ".dot"
targetDot dirName fileName = dirName ++ "/" ++ fileName ++ ".dot"

targetPng :: String -> String -> FilePath
targetPng "" fileName = fileName ++ ".png"
targetPng dirName fileName = dirName ++ "/" ++ fileName ++ ".png"

convertToId :: String -> Id
convertToId id = Id (T.pack id)

convertToNodeId :: String -> NodeId 
convertToNodeId id = NodeId (convertToId id) Nothing

createNode :: String -> String -> Statement
createNode id label = 
    StatementNode $ NodeStatement (convertToNodeId id) 
                                  -- [Attribute (convertToId "label") (convertToId label)]
                                  [Attribute (convertToId "label") (convertToId (" " ++ label ++ " "))]

createEdge :: String -> String -> Statement 
createEdge id1 id2 =
    StatementEdge $ EdgeStatement (ListTwo (EdgeNode (convertToNodeId id1)) 
                                           (EdgeNode (convertToNodeId id2)) 
                                           [])
                                  []


emptyGraph :: DotGraph
emptyGraph = DotGraph NonStrict Directed (Just (convertToId "")) []