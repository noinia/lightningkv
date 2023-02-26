module ThunderKV.Static.Tree
  ( Tree
  , FlatNode(..)
  , matchTree
  , asBinTree
  , toTree
  ) where

import           Control.DeepSeq
import           Data.Array (Array)
import qualified Data.Array as Array
import           GHC.Generics (Generic)
import           ThunderKV.BinTree
import           ThunderKV.Types


--------------------------------------------------------------------------------

-- | A binary tree embedded as a flat array.
type Tree = Array Index FlatNode

-- | The nodes of our embeded tree
data FlatNode = FlatLeaf {-# UNPACK #-} !Key {-# UNPACK #-} !Value
              | FlatNode {-# UNPACK #-} !Index
                         {-# UNPACK #-} !Key
                         {-# UNPACK #-} !Index
              deriving stock (Show,Read,Eq,Ord,Generic)
instance NFData FlatNode

-- | Construct a tree
toTree   :: Height -> [(a,FlatNode)] -> Tree
toTree h = Array.listArray (0,size h) . map snd

--------------------------------------------------------------------------------

-- | Pattern match on a Tree
matchTree             :: (Key -> Value -> r)
                      -> (r -> Key -> r -> r )
                      -> Tree
                      -> r
matchTree leaf node a = go 0
  where
    go i = case a Array.! i of
             FlatLeaf k v   -> leaf k v
             FlatNode l k r -> node (go l) k (go r)
{-# INLINE matchTree #-}

--------------------------------------------------------------------------------

-- | Convert the embedded tree into a normal Binary tree
asBinTree :: Tree -> BinTree Key Value
asBinTree = matchTree BinLeaf BinNode
{-# INLINE asBinTree #-}
