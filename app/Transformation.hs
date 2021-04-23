module Transformation (append) where

import Model


append :: Maybe Node->Int->Node
append tree val = do
  case tree of
    Nothing   -> Node { left = Nothing, right = Nothing, value = val }
    Just node -> do
      if val <= value node then
        Node { left  = Just $ append (left  node) val, right = right node, value = value node }
      else
        Node { right = Just $ append (right node) val, left = left node, value = value node }
