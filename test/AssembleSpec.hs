module AssembleSpec (assembleSpecs) where


import Model
import Assemble
import Navigation
import TestUtils
import Test.Tasty
import Test.Tasty.HUnit

assembleSpecs = [fromOneElement, fromExampleArray, fromOneElementBalanced, fromExampleArrayBalanced]

complexTreeCLR         = [30, 25, 5, 10, 30, 35, 50, 100, 60, 65]
complexTreeCLRBalanced = [30, 25, 5, 10, 30, 50, 35, 100, 60, 65]
complexTreeInput       = [30, 25, 35, 50, 30, 5, 100, 60, 65, 10]

fromOneElement = testCase "fromTree from one element tree"   $ assertTreeEqual "Must be a single element tree" [0] $ treeFrom [0]
fromExampleArray = testCase "fromTree from the example tree" $ assertTreeEqual "Must be a complex tree" complexTreeCLR $ treeFrom complexTreeInput

fromOneElementBalanced = testCase "fromTreeBalanced from one element tree" $ assertTreeEqual "Must be a single element tree"   [0] $ treeFromBalanced [0]
fromExampleArrayBalanced = testCase "fromTreeBalanced from the example"    $ assertTreeEqual "Must be a complex balanced tree" complexTreeCLRBalanced $ treeFromBalanced complexTreeInput