module Orientations (
  directionRelativeTo,
  directionRelativeTo'
) where

import Model
import Prelude hiding (Left, Right)
  
directionRelativeTo :: Node->Node->Direction
directionRelativeTo parent child = directionRelativeTo' (value parent) (value child)

directionRelativeTo' :: Int->Int->Direction
directionRelativeTo' parent child = if child < parent then Left else Right
