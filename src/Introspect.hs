module Introspect (find, depth, clr) where
  
import Model
import Navigation
import Orientations
import Prelude hiding (Left, Right)


find :: Maybe Node->Int->[Direction]->(Maybe Node, [Direction])
find node val stack = do
  case node of
    Nothing -> (Nothing, stack)
    Just x -> do
      if val == value x then
        (node, stack)
      else
        find (nodeAt direction x) val (stack ++ [direction]) where direction = directionRelativeTo' (value x) val


depth :: Maybe Node->Int->Int
depth node layer = do
  case node of
    Nothing -> layer
    Just x -> max (depth (left x) layer + 1) (depth (right x) layer + 1)


clr :: Maybe Node->[Int]->[Int]
clr mNode stack = case mNode of
  Nothing -> stack
  Just node -> clr (nodeAt Right node) $ clr (nodeAt Left node) (stack ++ [value node])
