module NavigationSpec (navigationSpecs) where


import Model
import Navigation
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (Left, Right)

baseTree = Node {
  value = 30,
  left = Just Node {
    value = 25,
    left = Just Node {
      value = 5,
      left = Just Node { value = 10, left = Nothing, right = Nothing },
      right = Nothing
    },
    right = Just Node {
      value = 30,
      left = Nothing,
      right = Nothing
    }
  },
  right = Just Node {
    value = 35,
    left = Nothing,
    right = Just Node {
      value = 50,
      left = Nothing,
      right = Just Node {
        value = 100,
        left = Nothing,
        right = Just Node {
          value = 60,
          left = Nothing,
          right = Just Node { value = 65, left = Nothing, right = Nothing }
        }
      }
    }
  }
}

navigationSpecs = [
  clrOnComplex, clrOnEmpty,
  depthOnComplex, depthOnSingle, depthOnEmpty,
  nodeAtLeft, nodeAtRight,
  orientToLeft, orientIfEquals, orientToRight,
  traceDirectionEmpty, traceDirectionSingleElement, traceDirectionComplex ]

orientToLeft   = testCase "Must be Left"  $ assertEqual "Because 30 > 25"  Left  $ directionRelativeTo 30 25
orientIfEquals = testCase "Must be Left"  $ assertEqual "Because 30 == 30" Left  $ directionRelativeTo 30 30
orientToRight  = testCase "Must be Right" $ assertEqual "Because 30 < 35"  Right $ directionRelativeTo 30 35

traceDirectionEmpty =         testCase "Trace the direction of empty array"     $ assertEqual "Must be empty" []     $ traceDirection []              []
traceDirectionSingleElement = testCase "Trace the direction of 1 element array" $ assertEqual "Must be empty" []     $ traceDirection [0]             []
traceDirectionComplex =       testCase "Trace the direction of a complex trace" $ assertEqual "" [Left, Left, Right] $ traceDirection [30, 25, 5, 10] []

clrOnComplex = testCase "CLR in the example tree" $ assertEqual "" [30, 25, 5, 10, 30, 35, 50, 100, 60, 65] $ clr (Just baseTree) []
clrOnEmpty   = testCase "CLR on empty tree"       $ assertEqual "Must be empty list" [] $ clr Nothing []

depthOnComplex = testCase "Depth of complex tree"     $ assertEqual "Must be 6" 6 $ depth (Just baseTree) 0
depthOnSingle  = testCase "Depth of single node tree" $ assertEqual "Must be 1" 1 $ depth (Just Node { value = 0, left = Nothing, right = Nothing }) 0
depthOnEmpty   = testCase "Depth of empty tree"       $ assertEqual "Must be 0" 0 $ depth Nothing 0

nodeAtLeft  = testCase "Node at the left of the root" $ assertEqual "Must be Just 25" (Just 25) $ case nodeAt Left baseTree of Nothing -> Nothing; Just x -> Just $ value x
nodeAtRight = testCase "Not at the right of the root" $ assertEqual "Must be Just 35" (Just 35) $ case nodeAt Right baseTree of Nothing -> Nothing; Just x -> Just $ value x
