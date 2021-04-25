module Main where

import OrientationSpec
import IntrospectSpec
  
import Test.Tasty
import Test.Tasty.HUnit


main :: IO ()
main = do
  defaultMain $ testGroup "Binary Tree" [
    testGroup "Orientation" orientationSpecs,
    testGroup "Introspect" introspectSpecs ]
  



