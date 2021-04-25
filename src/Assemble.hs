module Assemble where

import Model
import Transformation
  
treeFrom :: [Int]->Node
treeFrom [] = error "Empty Tree"
treeFrom (x:xs) = foldl (\tree val -> append (Just tree) val) Node { left = Nothing, right = Nothing, value = x } xs