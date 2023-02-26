module ThunderKV.Static
  ( Map
  , fromAscListPow2
  , lookup
  , lookupGE
  ) where

import           Control.DeepSeq
-- import           Control.Monad.Writer
import           Data.Array (Array)
import qualified Data.Array as Array
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Word
import           GHC.Generics (Generic)
import           Prelude hiding (lookup)

--------------------------------------------------------------------------------

type Index = Word64

newtype Key = Key Index
  deriving stock (Show,Read,Generic)
  deriving newtype (Eq,Ord,Bounded,Enum,NFData)

newtype Value = Value Index
  deriving stock (Show,Read,Generic)
  deriving newtype (Eq,Ord,Bounded,Enum,NFData)


--------------------------------------------------------------------------------

data FlatNode = FlatLeaf {-# UNPACK #-} !Key {-# UNPACK #-} !Value
              | FlatNode {-# UNPACK #-} !Index
                         {-# UNPACK #-} !Key
                         {-# UNPACK #-} !Index
              deriving stock (Show,Read,Eq,Ord,Generic)
instance NFData FlatNode








type Tree = Array Index FlatNode

--------------------------------------------------------------------------------


matchWith             :: (Key -> Value -> r)
                      -> (r -> Key -> r -> r )
                      -> Tree
                      -> r
matchWith leaf node a = go 0
  where
    go i = case a Array.! i of
             FlatLeaf k v   -> leaf k v
             FlatNode l k r -> node (go l) k (go r)

----------------------------------------

reconstruct :: Tree -> BinTree Key Value
reconstruct = matchWith BinLeaf BinNode

--------------------------------------------------------------------------------

newtype Map = Map Tree
  deriving stock (Show,Read,Generic)
  deriving newtype (Eq,Ord,NFData)


--------------------------------------------------------------------------------

data BinTree k v = BinLeaf k v
                 | BinNode (BinTree k v) k (BinTree k v)
                 deriving (Show,Eq,Functor,Foldable,Traversable)

instance Bifunctor BinTree where
  bimap f g = go
    where
      go = \case
        BinLeaf k v   -> BinLeaf (f k) (g v)
        BinNode l k r -> BinNode (go l) (f k) (go r)

instance Bifoldable BinTree where
  bifoldMap f g = go
    where
      go = \case
        BinLeaf k v   -> f k <> g v
        BinNode l k r -> go l <> f k <> go r

instance Bitraversable BinTree where
  bitraverse f g = go
    where
      go = \case
        BinLeaf k v   -> BinLeaf <$> f k <*> g v
        BinNode l k r -> BinNode <$> go l <*> f k <*> go r


leavesWithIndex :: BinTree k v -> [(Index, v)]
leavesWithIndex = zip [0..] . bifoldMap (const []) (: [])


mapLeavesWithIndex      :: Word64 -- h
                        -> (Index -> k -> v -> v')
                        -> BinTree k v -- size should be 2^h
                        -> BinTree k v'
mapLeavesWithIndex h0 g = go h0 0
  where
    go h s = \case
      BinLeaf k v   -> BinLeaf k (g s k v)
      BinNode l k r -> let go'   = go (h-1)
                           sizeL = 2 ^ (h-1)
                       in BinNode (go' s l) k (go' (s + sizeL) r)



-- mapLeavesWithIndex (heightL test) (\i v -> (i,v)) test



-- | computes the number of nodes h on the left-spine assuming the
-- tree is a complete binary tree, this then means the tree has 2^h
-- nodes.
heightL :: BinTree k v -> Word64
heightL = \case
  BinLeaf   _ _ -> 0
  BinNode l _ _ -> 1 + heightL l

-- | Returns the height of the top trees as well as the heights of the bottom trees
split   :: BinTree k v -> Either (k, v)
                                 (Word64, Word64, BinTree k (BinTree k v, BinTree k v))
split t = split' (heightL t) t

-- | invariant, tree has height 2^h
split'     :: Word64 -> BinTree k v -> Either (k, v)
                                              (Word64, Word64, BinTree k (BinTree k v, BinTree k v))
split' h t = case h of
               0 -> case t of
                      BinLeaf k v -> Left (k,v)
                      _           -> error "split' absurd leaf"
               _ -> let ht = h `div` 2
                        hb = if h == 1 then 0 else h - ht
                    in Right (ht, hb, go ht t)
  where
    go rh (BinNode l k r) = case rh of
                              0 -> BinLeaf k (l,r)
                              _ -> let go' = go (rh - 1)
                                   in BinNode (go' l) k (go' r)
    go _  _ = error "split': absurd node"


type Size = Index




layout :: BinTree Key Value -> [(Index,FlatNode)]
layout = layoutWith id id

layoutWith         :: (k -> Key) -> (v -> Value) -> BinTree k v -> [(Index,FlatNode)]
layoutWith mkK mkV = layout' mkK (\k v -> FlatLeaf (mkK k) (mkV v)) 0

layout'                :: (k -> Key)
                       -> (k -> v -> FlatNode)
                       -> Index
                       -> BinTree k v -> [(Index, FlatNode)]
layout' mkK mkNode s t =
  case split t of
    Left (k,v)       -> [(s, mkNode k v)]
    Right (ht,hb,top) -> let nt = 2 ^ ht -- size top
                             nb = 2 ^ hb -- size bottom
                             bottoms = concatMap (\(i,(b1,b2)) ->
                                                     layout' mkK mkNode (nt + 2*i*nb) b1
                                                     <>
                                                     layout' mkK mkNode (nt + (2*i+1)*nb) b2
                                                 )
                                     $ leavesWithIndex top
                             top' = layout' mkK (\_ v -> v) s
                                  $ mapLeavesWithIndex ht (\i k (b1,b2) ->
                                                              FlatNode (nt + 2*i*nb)
                                                                       (mkK k)
                                                                       (nt + (2*i+1)*nb)
                                                          ) top
                         in top' <> bottoms





fromAscListPow2' :: Ord k => NonEmpty (k,v) -> BinTree k v
fromAscListPow2' = fst . repeatedly merge . fmap (\(k,v) -> (BinLeaf k v, k))
  where
    repeatedly _ (t :| []) = t
    repeatedly f ts        = repeatedly f $ f ts

    merge ts@(_ :| [])  = ts
    merge (l :| r : []) = node l r :| []
    merge (l :| r : ts) = node l r NonEmpty.<| merge (NonEmpty.fromList ts)

    node (l,k) (r,m) = (BinNode l k r, m)


--------------------------------------------------------------------------------

test :: BinTree Word64 Word64
test = fromAscListPow2' $ NonEmpty.fromList
       [ (0,0)
       , (1,2)
       , (20,3)
       , (21,5)
       ]

test2 = fromAscListPow2' . NonEmpty.fromList . map (\i -> (Key i, Value i))
      $ [0..(2^3)-1]

--------------------------------------------------------------------------------

type Height = Index

-- -- | Construct a tree of size 2^h filled with zeros
-- construct   :: Word64 -> [FlatNode]
-- construct h = case h of
--                 0 -> [FlatLeaf (Key 0) (Value 0)]
--                 _ -> let ht = h `div` 2
--                          hb = h - ht
--                      in undefined



--------------------------------------------------------------------------------


fromAscListPow2 :: [(Key, Value)] -> Map
fromAscListPow2 = undefined


lookup     :: Key -> Map -> Maybe Value
lookup q m = case lookupGE q m of
               Just (q',v) | q == q' -> Just v
               _                     -> Nothing
{-# INLINE lookup #-}


lookupGE  :: Key -> Map -> Maybe (Key, Value)
lookupGE = undefined



--------------------------------------------------------------------------------

-- arrayFromList :: [(Index,a)] -> []



-- flatTest :: Tree
flatTest = layoutWith Key Value test

flatSimplest = layoutWith Key Value simplest


simplest :: BinTree Word64 Word64
simplest = BinNode (BinLeaf 0 1) 0 (BinLeaf 5 6)
