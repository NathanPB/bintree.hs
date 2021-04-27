module TestUtils (assertTreeEqual) where

import Navigation
import Model
import Assemble
import Test.Tasty.HUnit

assertTreeEqual :: String->[Int]->Node->Assertion
assertTreeEqual msg exp actual = assertEqual msg exp (clr (Just actual) [])
