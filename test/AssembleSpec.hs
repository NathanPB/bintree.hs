module AssembleSpec (assembleSpecs) where


import Model
import Assemble
import Introspect
import Test.Tasty
import Test.Tasty.HUnit

assembleSpecs = [fromOneElement, fromExampleArray]

oneElement = Node { value = 0, right = Nothing, left = Nothing }
complexTreeCLR = [30, 25, 5, 10, 30, 35, 50, 100, 60, 65]
complexTreeInput = [30, 25, 35, 50, 30, 5, 100, 60, 65, 10]

fromOneElement = testCase "fromTree from one element tree" $ assertEqual "One element tree" oneElement $ treeFrom [0]

fromExampleArray = testCase "fromTree from the example tree" $ assertEqual "Complex tree" complexTreeCLR $ clr (Just $ treeFrom complexTreeInput) []
