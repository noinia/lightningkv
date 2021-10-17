module Thunder.ExpTree
  where

import Thunder.Tree

--------------------------------------------------------------------------------

type Volume = Word
type Layer = Word

data ExpTree a b = ExpLeaf {-# UNPACK #-} b
                 | FatNode {-# UNPACK #-} !Layer
                           {-# UNPACK #-} !Volume
                                          (Tree VEB a (ExpTree a b))
                 deriving (Show,Eq)
