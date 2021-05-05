module Transformation (append, setNodeAt, deleteNode, rotate) where

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

rotate :: Node->Node

-- LL Rotation
rotate n1@(Node { left = Just n2@(Node { left = Just n3, right = Nothing }), right = Nothing}) = Node {
    left = Just n3,
    right = Just $ setNodeAt n1 Left Nothing,
    value = value n2
  }

-- RR Rotation
rotate n1@(Node { right = Just n2@(Node { right = Just n3, left = Nothing }), left = Nothing}) = Node {
  left = Just $ setNodeAt n1 Right Nothing,
  right = Just n3,
  value = value n2
  }

-- LR Rotation
rotate n1@(Node { left = Just n2@(Node { left = Nothing, right = Just n3 }), right = Nothing }) =
  rotate Node {
    left = Just Node {
      left = Just Node {
        left = left n3,
        right = right n3,
        value = value n2
      },
      right = Nothing,
      value = value n3
    },
    right = Nothing,
    value = value n1
  }

-- RL Rotation
rotate n1@(Node { right = Just n2@(Node { left = Just n3, right = Nothing }), left = Nothing }) =
  rotate Node {
    right = Just Node {
      right = Just Node {
        left = left n3,
        right = right n3,
        value = value n2
      },
      left = Nothing,
      value = value n3
    },
    left = Nothing,
    value = value n1
  }