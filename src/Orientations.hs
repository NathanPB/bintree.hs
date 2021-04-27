module Orientations (
  directionRelativeTo,
  traceDirection
) where

import Model
import Prelude hiding (Left, Right)

directionRelativeTo :: Int->Int->Direction
directionRelativeTo parent child = if child <= parent then Left else Right

traceDirection :: [Int]->[Direction]->[Direction]
traceDirection [] trace = trace
traceDirection (x:[]) trace = trace
traceDirection nodes@(parent:child:_) trace = traceDirection (tail nodes) (trace ++ [directionRelativeTo parent child])
