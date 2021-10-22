module Thunder.Tree.Refill
  (

  ) where

-- import WithMax
import qualified TreeAB_to_CWithMaxD
import qualified TreeAB_to_CD
import Thunder.Tree.Types

import qualified A
import qualified B
import qualified C
import qualified D
import CAsElem  -- imports CWithMax

--------------------------------------------------------------------------------

type A = A.NodeElem
type B = B.LeafElem
type C = C.NodeElem
type D = D.LeafElem

-- type A = TreeAB_to_CWithMaxD.A
-- type B = TreeAB_to_CWithMaxD.B
-- type C = TreeAB_to_CWithMaxD.C
-- type D = TreeAB_to_CWithMaxD.D

-- type TreeCWithMaxD = TreeAB_to_CWithMaxD.TreeCD

--------------------------------------------------------------------------------
-- * Filling Binary Search Trees

-- -- | Given a leaf to value function, an ascending list of elements xs,
-- -- and a tree t, create a BST by overwriting all values in the BST.
-- --
-- -- pre: length xs >= capacity t
-- reFillWith      :: (D -> C) -> [D] -> TreeAB l -> TreeCD l
-- reFillWith f xs = bimapTree (\(WithMax x _) -> x) id . fillWith f xs

-- | Given a leaf to key function and a list of leaf values,
-- "overwrite" the values in the input tree by those in the input list
-- (in increasing order). Note that this actually constructs a new
-- array in memory sotring this tree.

-- This function is useful when the input tree has the right
-- structure/memory layout.
fillWith   :: (D -> C) -> [D] -> TreeAB_to_CWithMaxD.TreeAB l -> TreeAB_to_CWithMaxD.TreeCD l
fillWith f = TreeAB_to_CWithMaxD.fillWith' (\(WithMax _ lM) _ (WithMax _ rM) -> WithMax lM rM)
                                           (\d _ -> d)
                                           (\d -> let c = f d in WithMax c c)


--------------------------------------------------------------------------------
