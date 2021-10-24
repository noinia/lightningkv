module Foo where

import WithMax
-- import NodeElemAsMaxElem
import NodeElem
-- import qualified NodeElemAsMaxElem

type NodeElemWithMax = WithMax

-- foo :: NodeElemWithMax -> String
-- foo = \case
--   NodeElemWithMax.Leaf _ -> Nothing
--   NodeElemWithMax.Node _ (NodeElemWithMax.WithMax)

theMaxOfAWithMax               :: WithMax -> NodeElem -- WithMax -> MaxElem
theMaxOfAWithMax (WithMax _ x) = x
