module TestUtils (assertTreeEqual, assertTreeEqual') where

import Navigation
import Model
import Assemble
import Test.Tasty.HUnit

assertTreeEqual :: String->[Int]->Node->Assertion
assertTreeEqual msg exp actual = assertEqual msg exp $ clr (Just actual)

assertTreeEqual' :: String->Maybe [Int]->Maybe Node->Assertion
assertTreeEqual' msg (Nothing) (Nothing) = assertEqual msg (Nothing :: Maybe [Int]) Nothing
assertTreeEqual' msg (Just a)  (Just b)  = assertTreeEqual msg a b
assertTreeEqual' msg (Nothing) (Just b)  = assertEqual msg Nothing $ Just $ clr $ Just b
assertTreeEqual' msg (Just a)  (Nothing) = assertEqual msg (Just a) Nothing