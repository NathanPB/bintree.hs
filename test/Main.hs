module Main where

import OrientationSpec
import NavigationSpec
import AssembleSpec
  
import Test.Tasty
import Test.Tasty.HUnit


main :: IO ()
main = do
  defaultMain $ testGroup "Binary Tree" [
    testGroup "Orientation" orientationSpecs,
    testGroup "Navigation" navigationSpecs,
    testGroup "Assemble" assembleSpecs]
  



