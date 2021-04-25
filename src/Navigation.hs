module Navigation (
  nodeAt,
  navigate,
  navigate',
) where

import Model
import Orientations
import Prelude hiding (Left, Right)

nodeAt :: Direction->Node->Maybe Node
nodeAt direction = (if direction == Left then left else right)
  
navigate :: Node->[Direction]->Maybe Node
navigate parent [] = Just parent
navigate parent (d:ds) = case nodeAt d parent of
    Nothing -> Nothing
    Just nextNode -> navigate nextNode ds
  
navigate' :: [Node]->[Direction]->[Node]
navigate' parent [] = parent
navigate' stack@[parent] (d:ds) = case nodeAt d parent of
  Nothing -> stack
  Just x  -> navigate' (stack ++ [x]) ds
