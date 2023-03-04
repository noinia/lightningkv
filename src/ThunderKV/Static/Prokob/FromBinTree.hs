-- Implementation via recursively splitting a normal, pointer based
-- binary tree.
module ThunderKV.Static.Prokob.FromBinTree
  ( fromAscListPow2
  , split
  , layout
  , layoutWith
  ) where

import qualified Data.List.NonEmpty as NonEmpty
import qualified ThunderKV.Static.BinTree as BinTree
import           ThunderKV.Static.BinTree (BinTree(..))
import           ThunderKV.Static.Tree
import           ThunderKV.Static.Types

--------------------------------------------------------------------------------
-- | Constructs a complete bin-tree from a list given in ascdending
-- order.
--
-- pre: input has size 2^h, for some h >= 0
fromAscListPow2      :: Height -> NonEmpty.NonEmpty (Key,Value) -> Tree
fromAscListPow2 h = fromNonEmpty h . NonEmpty.fromList . fmap snd
                  . layout
                  . BinTree.fromAscListPow2


-- | Returns the height of the top trees as well as the heights of the bottom trees
split   :: BinTree k v -> Either (k, v)
                                 (Height, Height, BinTree k (BinTree k v, BinTree k v))
split t = split' (BinTree.heightL t) t

-- | invariant, tree has height 2^h
split'     :: Height -> BinTree k v -> Either (k, v)
                                              (Height, Height, BinTree k (BinTree k v, BinTree k v))
split' h t = case h of
               0 -> case t of
                      BinLeaf k v -> Left (k,v)
                      _           -> error "split' absurd leaf"
               _ -> let ht = h `div` 2
                        hb = if even h then ht - 1 else ht
                    in Right (ht, hb, go ht t)
  where
    go rh (BinNode l k r) = case rh of
                              0 -> BinLeaf k (l,r)
                              _ -> let go' = go (rh - 1)
                                   in BinNode (go' l) k (go' r)
    go _  _ = error "split': absurd node"


layout :: BinTree Key Value -> [(Index,FlatNode)]
layout = layoutWith id id

layoutWith         :: (k -> Key) -> (v -> Value) -> BinTree k v -> NodeWriter
layoutWith mkK mkV = layout' mkK (\k v -> FlatLeaf (mkK k) (mkV v)) 0

type NodeWriter = [(Index, FlatNode)]


layout'                :: (k -> Key)
                       -> (k -> v -> FlatNode)
                       -> Index
                       -> BinTree k v -> NodeWriter
layout' mkK mkNode s t =
    case split t of
      Left (k,v)       -> [(s, mkNode k v)]
      Right (ht,hb,top) ->
        let nt = size ht -- tree top    tree (in number of nodes)
            nb = size hb -- size bottom tree (in number of nodes)
            WithComp bottoms top' = BinTree.itraverseWithKey f top

            f i k (b1,b2) = let li    = s + nt + 2*i*nb
                                ri    = s + nt + (2*i+1)*nb
                                node' = FlatNode li (mkK k) ri
                                comp = layout' mkK mkNode li b1
                                       <>
                                       layout' mkK mkNode ri b2
                            in WithComp comp node'
        in layout' mkK (\_ v -> v) s top' <> bottoms

data WithComp a = WithComp NodeWriter a deriving (Functor)

instance Applicative WithComp where
  pure = WithComp []
  (WithComp comp f) <*> (WithComp comp' x) = WithComp (comp <> comp') (f x)
