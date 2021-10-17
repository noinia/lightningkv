module Thunder.Node where

import qualified NodeAB
import qualified NodeCD

type A = NodeAB.NodeElem
type B = NodeAB.LeafElem
type C = NodeCD.NodeElem
type D = NodeCD.LeafElem

-- | Bimap for nodes
bimapNode     :: (A -> C) -> (B -> D) -> NodeAB.Node -> NodeCD.Node
bimapNode f g = \case
  NodeAB.Leaf b     -> NodeCD.Leaf (g b)
  NodeAB.Node l a r -> NodeCD.Node l (f a) r
