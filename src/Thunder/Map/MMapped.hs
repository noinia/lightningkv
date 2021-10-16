module Thunder.Map.MMapped
  ( storeTo
  , loadFrom
  ) where


import qualified Data.Vector.Storable as SV
import           Data.Vector.Storable.MMap
import           Foreign.Storable
import qualified Thunder.Map.Base as Base
import qualified Thunder.Map.Storable
import           Thunder.Tree

--------------------------------------------------------------------------------

-- |
storeTo                            :: Storable v => FilePath -> Map k v -> IO ()
storeTo fp (Base.Map (Tree tv) vs) = do writeMMapVector (treeFile fp)  tv
                                        writeMMapVector (valueFile fp) vs

loadFrom    :: Storable v => FilePath -> IO (Map k v)
loadFrom fp = Base.Map <$> (Tree <$> unsafeMMapVector (treeFile fp)  Nothing)
                       <*> (         unsafeMMapVector (valueFile fp) Nothing)

--------------------------------------------------------------------------------

treeFile fp  = fp <> ".tree"
valueFile fp = fp <> ".values"


--------------------------------------------------------------------------------

testMap :: Map Int Char
testMap = Base.fromAscList $ zip [0..10] ['a'..'z']


loadTestMap :: IO (Map Int Char)
loadTestMap = loadFrom "/tmp/testMap"
