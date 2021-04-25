module IntrospectSpec (introspectSpecs) where


import Model
import Introspect
import Test.Tasty
import Test.Tasty.HUnit

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

introspectSpecs = [ clrOnComplex, clrOnEmpty, depthOnComplex, depthOnSingle, depthOnEmpty ]

clrOnComplex = testCase "CLR in the example tree" $ assertEqual "" [30, 25, 5, 10, 30, 35, 50, 100, 60, 65] $ clr (Just baseTree) []

clrOnEmpty = testCase "CLR on empty tree" $ assertEqual "Must be empty list" [] $ clr Nothing []

depthOnComplex = testCase "Depth of complex tree" $ assertEqual "Must be 6" 6 $ depth (Just baseTree) 0

depthOnSingle = testCase "Depth of single node tree" $ assertEqual "Must be 1" 1 $ depth (Just Node { value = 0, left = Nothing, right = Nothing }) 0

depthOnEmpty = testCase "Depth of empty tree" $ assertEqual "Must be 0" 0 $ depth Nothing 0