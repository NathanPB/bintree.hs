module Navigation (
  nodeAt,
  navigateWith, navigateWith',
  find,
  depth,
  clr,
  directionRelativeTo,
  balanceFactor
) where

import Model
import Prelude hiding (Left, Right)

directionRelativeTo :: Int->Int->Direction
directionRelativeTo parent child
 | child <= parent = Left
 | otherwise       = Right

find :: Maybe Node->Int->[Direction]->(Maybe Node, [Direction])
find Nothing _ stack    = (Nothing, stack)
find (Just x) val stack
 | val == value x = (Just x, stack)
 | otherwise = find (nodeAt direction x) val (stack ++ [direction])
     where direction = directionRelativeTo (value x) val

depth :: Maybe Node->Int->Int
depth node layer = do
  case node of
    Nothing -> layer
    Just x -> max (depth (left x) layer + 1) (depth (right x) layer + 1)

clr :: Maybe Node->[Int]
clr Nothing     = []
clr (Just node) = [value node] ++ (clr $ left node) ++ (clr $ right node)

nodeAt :: Direction->Node->Maybe Node
nodeAt Left  = left
nodeAt Right = right
  
navigateWith :: Node->[Direction]->Maybe Node
navigateWith parent [] = Just parent
navigateWith parent (d:ds) = case nodeAt d parent of
    Nothing -> Nothing
    Just nextNode -> navigateWith nextNode ds
    
navigateWith' :: Node->[Direction]->Node
navigateWith' node [] = node
navigateWith' node (d:ds) = case nodeAt d node of
  Nothing -> node
  Just children -> navigateWith' children ds

balanceFactor :: Node->Int
balanceFactor (Node { left = Just n,  right = Nothing }) = depth (Just n) 0
balanceFactor (Node { right = Just n, left = Nothing  }) = depth (Just n) 0
balanceFactor (Node { left = Nothing, right = Nothing }) = 0
balanceFactor (Node { left = Just l,  right = Just r  }) = (balanceFactor l) - (balanceFactor r)