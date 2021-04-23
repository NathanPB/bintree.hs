module Main where

import Data.Maybe (isNothing)

data Node = Node { value :: Int, left :: Maybe Node, right :: Maybe Node } deriving Show


indent :: String->Int->String
indent s 0 = s
indent s level = "  " ++ indent s (level - 1)

pp :: Maybe Node->Int->String
pp node level = do
  case node of
    Nothing -> indent "|--> Nothing" level
    Just x -> do
      if isLeaf x then
        indent ("|--> " ++ show (value x)) level
      else
        indent ("|--> " ++ show (value x) ++ "\n" ++ pp (left x) (level + 1) ++ "\n" ++ pp (right x) (level + 1)) level

isLeaf :: Node->Bool
isLeaf x = isNothing (left x) && isNothing (right x)

append :: Maybe Node->Int->Node
append tree val = do
  case tree of
    Nothing   -> Node { left = Nothing, right = Nothing, value = val }
    Just node -> do
      if val <= value node then
        Node { left  = Just $ append (left  node) val, right = right node, value = value node }
      else
        Node { right = Just $ append (right node) val, left = left node, value = value node }

find :: Maybe Node->Int->[(String, Maybe Int)]->[(String, Maybe Int)]
find node val stack = do
  case node of
    Nothing -> stack ++ [("Not Found", Nothing)]
    Just x -> do
      if val == value x then
        stack ++ [("Done", Just $ value x)]
      else if val < value x then
        find (left x) val (stack ++ [("Left", Just $ value x)])
      else
        find (right x) val (stack ++ [("Right", Just $ value x)])

depth :: Maybe Node->Int->Int
depth node layer = do
  case node of
    Nothing -> layer
    Just x -> max (depth (left x) layer + 1) (depth (right x) layer + 1)

main :: IO ()
main = do
  let tree = (foldl (\tree val -> Just $ append tree val) Nothing [30, 25, 35, 50, 30, 5, 100, 60, 65, 10])
  print $ find tree 63 []
  putStr $ pp tree 0
  print $ depth tree 0
  -- putStr $ pp $ append Just (append Just (append Just (append Just (append Nothing 2) 3) 4) 1) 3
