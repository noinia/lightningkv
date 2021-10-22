module Thunder.Node(bimapNode) where

import qualified NodeAB
import qualified NodeCD
import qualified A
import qualified B
import qualified C
import qualified D

type A = A.NodeElem
type B = B.LeafElem
type C = C.NodeElem
type D = D.LeafElem

-- | Bimap for nodes
bimapNode     :: (A -> C) -> (B -> D) -> NodeAB.Node -> NodeCD.Node
bimapNode f g = \case
  NodeAB.Leaf b     -> NodeCD.Leaf (g b)
  NodeAB.Node l a r -> NodeCD.Node l (f a) r
