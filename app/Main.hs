{-# LANGUAGE  TupleSections #-}
module Main where

import System.Environment(getArgs)
import System.Random
import Control.Monad(replicateM)
import Lightning.Prokob
import qualified Lightning.Map as Map
import Data.List(scanl1)

--------------------------------------------------------------------------------

genAscList   :: Height -> IO (Size, [Int])
genAscList h = (\xs -> (n,scanl1 (+) xs)) <$> replicateM n (randomRIO (0,10))
  where
    n = fromIntegral $ pow2 h


buildMap    :: FilePath -> IO (Map.Map Int Int)
buildMap fp = do (n:xs) <- map read . lines <$> readFile fp
                 let ys = map (\x -> (x,x)) xs
                 pure $ Map.fromAscListN n ys

genFile hs = do let h = read hs
                (n,xs) <- genAscList h
                print n
                mapM_ print xs


mainWith [m, hs] = case m of
                     "gen"   -> genFile hs
                     "build" -> do m <- buildMap hs
                                   print $ m

main :: IO ()
main = getArgs >>= mainWith
