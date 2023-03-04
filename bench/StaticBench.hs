module Main where

import           Control.DeepSeq
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as ContainersMap
import           Input
import           Test.QuickCheck
import           Test.Tasty.Bench
import qualified ThunderKV.Static.Map as Map
import           ThunderKV.Static.Types

--------------------------------------------------------------------------------

fibo :: Int -> Integer
fibo n = if n < 2 then toInteger n else fibo (n - 1) + fibo (n - 2)

revInput               :: Inputs -> Inputs
revInput (Inputs h xs) = Inputs h $ force $ NonEmpty.reverse xs

pattern InputRev :: Height -> NonEmpty.NonEmpty (Key,Value) -> Inputs
pattern InputRev h xs <- (revInput -> Inputs h xs)
{-# COMPLETE InputRev #-}

-- generateQueres :: Gen a -> Gen [a]


main :: IO ()
main = do
  maxInput <- generate $ arbitrarySized 20
  queries <- generate infiniteList
  let inputs = drop 9 $ shrink maxInput
  defaultMain $
    for inputs $ \(InputRev h xs) ->
      bgroup ("of size 2^" <> show h)
        [ bgroup "Building map" $
            [ bench "Static.Map"     $ nf (Map.fromDescListPow2 h) xs
            , bench "Containers.Map" $ nf ContainersMap.fromDescList (F.toList xs)
            ]
        , bgroup "successor (lookupGE) queries" $
           let !thunderMap    = Map.fromDescListPow2 h xs
               !containerMap  = ContainersMap.fromDescList (F.toList xs)
               queries' = take (2 ^ h) $ filter (inRange h) queries
               runQueries             :: (Key -> map -> Maybe (Key,Value))
                                      -> map
                                      -> [Maybe (Key,Value)]
               runQueries qAlg theMap = map (\q -> qAlg q theMap) queries'
           in
            [ bench "Static.Map"     $ nf (runQueries Map.lookupGE)            thunderMap
            , bench "Containers.Map" $ nf (runQueries ContainersMap.lookupGE)  containerMap
            ]
        ]

inRange           :: Height -> Key -> Bool
inRange h (Key x) = 0 <= x && x <= 2 ^ h


for :: [a] -> (a -> b) -> [b]
for = flip map
