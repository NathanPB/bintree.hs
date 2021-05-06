module Assemble where

import Model
import Transformation
  
treeFrom :: [Int]->Node
treeFrom [] = error "Empty Tree"
treeFrom (x:xs) = foldl (append.Just) (Node { left = Nothing, right = Nothing, value = x }) xs

treeFromBalanced :: [Int]->Node
treeFromBalanced [] = error "Empty Tree"
treeFromBalanced (x:xs) = foldl (appendBalanced.Just) (Node { left = Nothing, right = Nothing, value = x }) xs