module Transformation (append) where

import Model
import Navigation
import Prelude hiding (Left, Right)


append :: Maybe Node->Int->Node
append tree val = do
  case tree of
    Nothing   -> Node { left = Nothing, right = Nothing, value = val }
    Just node -> do
      if (directionTo (value node) val) == Left then
        Node { left  = Just $ append (left  node) val, right = right node, value = value node }
      else
        Node { right = Just $ append (right node) val, left = left node, value = value node }
