module ThunderKV.Prokob
  ( split
  , layout
  , layoutWith
  ) where

-- import           Control.Monad.Writer
-- import           Data.List.NonEmpty (NonEmpty(..))
-- import qualified Data.List.NonEmpty as NonEmpty
import ThunderKV.Types
import ThunderKV.BinTree
import ThunderKV.Static.Tree
import Debug.Trace

----------------------------------------

--------------------------------------------------------------------------------



-- mapLeavesWithIndex (heightL test) (\i v -> (i,v)) test




-- | Returns the height of the top trees as well as the heights of the bottom trees
split   :: BinTree k v -> Either (k, v)
                                 (Height, Height, BinTree k (BinTree k v, BinTree k v))
split t = split' (heightL t) t

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
        let nt = size ht -- size top
            nb = size hb -- size bottom
            WithComp bottoms top' = itraverseWithKey f top

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



                         --     bottoms = concatMap (\(i,(b1,b2)) ->
                         --                             layout' mkK mkNode (nt + 2*i*nb) b1
                         --                             <>
                         --                             layout' mkK mkNode (nt + (2*i+1)*nb) b2
                         --                         )
                         --             $ leavesWithIndex top
                         --     top' = layout' mkK (\_ v -> v) s
                         --          $ imapLeavesWithKey ht (\i k (b1,b2) ->
                         --                                      FlatNode (nt + 2*i*nb)
                         --                                               (mkK k)
                         --                                               (nt + (2*i+1)*nb)
                         --                                  ) top
                         -- in top' <> bottoms

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------


-- -- | Construct a tree of size 2^h filled with zeros
-- construct   :: Word64 -> [FlatNode]
-- construct h = case h of
--                 0 -> [FlatLeaf (Key 0) (Value 0)]
--                 _ -> let ht = h `div` 2
--                          hb = h - ht
--                      in undefined



--------------------------------------------------------------------------------





--------------------------------------------------------------------------------

-- arrayFromList :: [(Index,a)] -> []



-- flatTest :: Tree
-- flatTest = layoutWith Key Value test
