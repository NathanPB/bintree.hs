module Navigation (
  nodeAt,
  navigate, navigate',
  find,
  findTraced, findTraced',
  depth,
  clr
) where

import Model
import Orientations
import Prelude hiding (Left, Right)

find :: Maybe Node->Int->[Direction]->(Maybe Node, [Direction])
find Nothing _ stack    = (Nothing, stack)
find (Just x) val stack
 | val == value x = (Just x, stack)
 | otherwise = find (nodeAt direction x) val (stack ++ [direction])
     where direction = directionRelativeTo (value x) val

findTraced' :: Node->Int->[Node]->[Node]
findTraced' node val trace =
  if val == value node then trace else
  case nodeAt (directionRelativeTo (value node) val) node of
    Nothing -> trace
    Just nextNode -> findTraced' nextNode val $ trace ++ [nextNode]

findTraced :: Node->Int->[Direction]
findTraced node val = traceDirection (map value (findTraced' node val [])) []

depth :: Maybe Node->Int->Int
depth node layer = do
  case node of
    Nothing -> layer
    Just x -> max (depth (left x) layer + 1) (depth (right x) layer + 1)


clr :: Maybe Node->[Int]->[Int]
clr Nothing stack                                          = stack
clr (Just (Node { left = l, right = r, value = x })) stack = clr r $ clr l $ stack ++ [x]

nodeAt :: Direction->Node->Maybe Node
nodeAt Left  = left
nodeAt Right = right
  
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
