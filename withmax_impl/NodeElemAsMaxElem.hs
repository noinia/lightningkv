module NodeElemAsMaxElem where

import NodeElem

type MaxElem  = NodeElem
type Elem     = NodeElem


-- since NodeElem implements Eq this should not be any issue.
reflexivity :: NodeElem -> Bool
reflexivity x = x == x
