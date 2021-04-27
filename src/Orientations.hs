module Orientations (
  directionRelativeTo, directionRelativeTo',
  traceDirection, traceDirection'
) where

import Model
import Prelude hiding (Left, Right)
  
directionRelativeTo :: Node->Node->Direction
directionRelativeTo parent child = directionRelativeTo' (value parent) (value child)

directionRelativeTo' :: Int->Int->Direction
directionRelativeTo' parent child = if child <= parent then Left else Right

traceDirection :: [Node]->[Direction]
traceDirection [] = []
traceDirection (_:[]) = []
traceDirection nodes = traceDirection' (map value nodes) []

traceDirection' :: [Int]->[Direction]->[Direction]
traceDirection' [] trace = trace
traceDirection' (x:[]) trace = trace
traceDirection' nodes@(parent:child:_) trace = traceDirection' (tail nodes) (trace ++ [directionRelativeTo' parent child])
