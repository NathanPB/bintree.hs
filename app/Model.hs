module Model (
  Node(..),
  Direction(..)
) where

import Prelude hiding (Left, Right)

data Node = Node { value :: Int, left :: Maybe Node, right :: Maybe Node } deriving Show
data Direction = Left | Right deriving (Show, Eq)
