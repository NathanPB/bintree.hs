module TransformationSpec (transformationSpecs) where

import Model
import Transformation
import Assemble
import Test.Tasty
import Test.Tasty.HUnit
import TestUtils
import Prelude hiding (Left, Right)

transformationSpecs = [
  deleteLeaf, deletePartialBranch, deleteFullBranch,
  setNodeAtLeft, setNodeAtRight, setNothingAtLeft, setNothingAtRight,
  llRotation, rrRotation, lrRotation, rlRotation]

baseTree = treeFrom [30, 25, 35, 50, 30, 5, 100, 60, 65, 10]

deleteLeaf = testCase "Delete a leaf from a tree" $
  assertTreeEqual' "Must be the tree without the leaf of value 10" (Just [30, 25, 5, 30, 35, 50, 100, 60, 65]) $
  deleteNode baseTree [Left, Left, Right]

deletePartialBranch = testCase "Delete a node with a single child from a tree" $
  assertTreeEqual' "Must be the tree without the node of value 5" (Just [30, 25, 10, 30, 35, 50, 100, 60, 65]) $
  deleteNode baseTree [Left, Left]

deleteFullBranch = testCase "Delete a node with two children from a tree" $
  assertTreeEqual' "Must be the tree without the node of value 30" (Just [35, 25, 5, 10, 30, 50, 100, 60, 65]) $
  deleteNode baseTree []

setNodeAtLeft = testCase "Set the node at left to 29" $
  assertTreeEqual "Must be the root tree with a node valued 29 at the left" [30, 29, 35, 50, 100, 60, 65] $
  setNodeAt baseTree Left (Just Node { left = Nothing, right = Nothing, value = 29 })

setNodeAtRight = testCase "Set the node at right to 35" $
  assertTreeEqual "Must be the root tree with a node valued 31 at the right" [30, 25, 5, 10, 30, 31] $
  setNodeAt baseTree Right (Just Node { left = Nothing, right = Nothing, value = 31 })

setNothingAtLeft = testCase "Remove the node at the left" $
  assertTreeEqual "Must be the root tree without the left branch" [30, 35, 50, 100, 60, 65] $
  setNodeAt baseTree Left Nothing

setNothingAtRight = testCase "Remove the node at the right" $
  assertTreeEqual "Must be the root tree without the right branch" [30, 25, 5, 10, 30] $
  setNodeAt baseTree Right Nothing

llRotation = testCase "Performs a LL rotation" $
  assertTreeEqual "Must be the same tree, but balanced" [20, 10, 30] $
  rotate $ treeFrom [30, 20, 10]

rrRotation = testCase "Performs a RR rotation" $
  assertTreeEqual "Must be the same tree, but balanced" [20, 10, 30] $
  rotate $ treeFrom [10, 20, 30]

lrRotation = testCase "Performs a LR rotation" $
  assertTreeEqual "Must be the same tree, but balanced" [20, 10, 30] $
  rotate $ treeFrom [30, 10, 20]

rlRotation = testCase "Performs a RL rotation" $
  assertTreeEqual "Must be the same tree, but balanced" [20, 10, 30] $
  rotate $ treeFrom [10, 30, 20]
