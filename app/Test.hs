module Main where

import Thunder.Node
import qualified NodeCD
import qualified NodeCWithMaxD

myNode :: NodeCD.Node
myNode = NodeCD.Node 1 5 97


myMaxNode = MyMaxNode.Node 1 (WithMax 5 5) 9

-- myCDNode :: NodeCD.Node
-- myCDNode = bimapNode id toEnum myNode



main = do print myNode
          print myMaxNode
