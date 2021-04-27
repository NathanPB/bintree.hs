module Transformation (append) where

import Model
import Orientations
import Prelude hiding (Left, Right)


append :: Maybe Node->Int->Node
append Nothing val = Node { left = Nothing, right = Nothing, value = val }
append (Just node) val = case (directionRelativeTo (value node) val) of
  Left  -> Node { left  = Just $ append (left  node) val, right = right node, value = value node }
  Right -> Node { right = Just $ append (right node) val, left = left node,   value = value node }
