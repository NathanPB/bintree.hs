module Navigation (find, depth, navigate) where

import Model
import Prelude hiding (Left, Right)


find :: Maybe Node->Int->[Direction]->(Maybe Node, [Direction])
find node val stack = do
  case node of
    Nothing -> (Nothing, stack)
    Just x -> do
      if val == value x then
        (node, stack)
      else if val < value x then
        find (left x) val (stack ++ [Left])
      else
        find (right x) val (stack ++ [Right])

depth :: Maybe Node->Int->Int
depth node layer = do
  case node of
    Nothing -> layer
    Just x -> max (depth (left x) layer + 1) (depth (right x) layer + 1)

navigateOnce :: Node->Direction->Maybe Node
navigateOnce node direction = if direction == Left then left node else right node

navigate :: [Maybe Node]->[Direction]->[Maybe Node]
navigate mTree [] = mTree
navigate mTree directions = do
  case last mTree of
    Nothing -> mTree ++ [Nothing]
    Just tree -> case navigateOnce tree $ head directions of
      Nothing -> mTree ++ [Nothing]
      Just child -> navigate (mTree ++ [Just child]) (tail directions)

findParentOf :: Maybe Node->Node->Maybe Node
findParentOf mTree node = mTree

