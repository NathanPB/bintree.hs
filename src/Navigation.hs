module Navigation (
  nodeAt,
  navigateWith, navigateWith',
  find,
  findTraced, findTraced',
  depth,
  clr,
  directionRelativeTo,
  traceDirection
) where

import Model
import Prelude hiding (Left, Right)

directionRelativeTo :: Int->Int->Direction
directionRelativeTo parent child
 | child <= parent = Left
 | otherwise       = Right

traceDirection' :: [Int]->[Direction]->[Direction]
traceDirection' [] trace = trace
traceDirection' (x:[]) trace = trace
traceDirection' nodes@(parent:child:_) trace = traceDirection' (tail nodes) (trace ++ [directionRelativeTo parent child])

traceDirection :: [Int]->[Direction]
traceDirection [] = (flip traceDirection') []

find :: Maybe Node->Int->[Direction]->(Maybe Node, [Direction])
find Nothing _ stack    = (Nothing, stack)
find (Just x) val stack
 | val == value x = (Just x, stack)
 | otherwise = find (nodeAt direction x) val (stack ++ [direction])
     where direction = directionRelativeTo (value x) val

findTraced' :: Node->Int->[Node]->[Node]
findTraced' node val trace
 | val == value node = trace
 | otherwise = case nodeAt (directionRelativeTo (value node) val) node of
   Nothing -> trace
   Just nextNode -> findTraced' nextNode val $ trace ++ [nextNode]

findTraced :: Node->Int->[Direction]
findTraced node val = traceDirection (map value (findTraced' node val [])) []

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
