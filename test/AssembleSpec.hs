module AssembleSpec (assembleSpecs) where


import Model
import Assemble
import Navigation
import TestUtils
import Test.Tasty
import Test.Tasty.HUnit

assembleSpecs = [fromOneElement, fromExampleArray]

complexTreeCLR   = [30, 25, 5, 10, 30, 35, 50, 100, 60, 65]
complexTreeInput = [30, 25, 35, 50, 30, 5, 100, 60, 65, 10]

fromOneElement = testCase "fromTree from one element tree"   $ assertTreeEqual "Must be a single element tree" [0] $ treeFrom [0]
fromExampleArray = testCase "fromTree from the example tree" $ assertTreeEqual "Must be a complex tree" complexTreeCLR $ treeFrom complexTreeInput
