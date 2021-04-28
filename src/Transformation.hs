module Transformation (append, setNodeAt, deleteNode) where

import Model
import Navigation
import Prelude hiding (Left, Right)


append :: Maybe Node->Int->Node
append Nothing val = Node { left = Nothing, right = Nothing, value = val }
append (Just node) val = case (directionRelativeTo (value node) val) of
  Left  -> Node { left  = Just $ append (left  node) val, right = right node, value = value node }
  Right -> Node { right = Just $ append (right node) val, left = left node,   value = value node }

setNodeAt :: Node->Direction->Maybe Node->Node
setNodeAt root Left  to = Node { left = to,  right = right root, value = value root }
setNodeAt root Right to = Node { right = to, left = left root,   value = value root }

deleteNode :: Node->[Direction]->Maybe Node
deleteNode (Node { left  = Nothing, right = Nothing })        [] = Nothing
deleteNode (Node { left  = Just onlyChild, right = Nothing }) [] = Just onlyChild
deleteNode (Node { right = Just onlyChild, left  = Nothing }) [] = Just onlyChild
deleteNode (Node { left = Just l, right = Just r })           [] = Just Node {
  value = value $ lowestNode,
  left = Just l,
  right = right lowestNode
} where lowestNode = navigateWith' r $ repeat Left

deleteNode node (direction:directionStack) = case (nodeAt direction node) of
  Nothing -> Nothing
  Just x  -> Just $ setNodeAt node direction $ deleteNode x directionStack
