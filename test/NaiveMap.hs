module NaiveMap( Map
               , fromDescListPow2
               , lookupGE
               , lookupLE
               ) where


import qualified Data.Foldable as F
import           Data.List.NonEmpty (NonEmpty)
import           Data.Ord (comparing)

type Map k v = [(k,v)]


fromDescListPow2   :: height -> NonEmpty (k,v) -> Map k v
fromDescListPow2 _ = F.toList

lookupGE      :: Ord k => k -> Map k v -> Maybe (k,v)
lookupGE q xs = minimumM (comparing fst) [ t | t@(k,_) <- xs, q <= k]

lookupLE      :: Ord k => k -> Map k v -> Maybe (k,v)
lookupLE q xs = maximumM (comparing fst) [ t | t@(k,_) <- xs, q >= k]


type Comparing a = a -> a -> Ordering

minimumM     :: Comparing a -> [a] -> Maybe a
minimumM cmp = \case
  [] -> Nothing
  xs -> Just (F.minimumBy cmp xs)

maximumM     :: Comparing a -> [a] -> Maybe a
maximumM cmp = \case
  [] -> Nothing
  xs -> Just (F.maximumBy cmp xs)
