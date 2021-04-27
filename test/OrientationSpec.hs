module OrientationSpec (orientationSpecs) where

import Model
import Orientations
import Prelude hiding (Left, Right)
import Test.Tasty
import Test.Tasty.HUnit

leftNode = Node { value = 25, left = Nothing, right = Nothing }
rightNode = Node { value = 35, left = Nothing, right = Nothing }
parentNode = Node { value = 30, left = Just leftNode, right = Just rightNode }

orientationSpecs = [ 
  orientToLeft, orientIfEquals, orientToRight,
  traceDirectionEmpty, traceDirectionSingleElement, traceDirectionComplex ]


orientToLeft   = testCase "Must be Left"  $ assertEqual "Because 30 > 25"  Left  $ directionRelativeTo 30 25
orientIfEquals = testCase "Must be Left"  $ assertEqual "Because 30 == 30" Left  $ directionRelativeTo 30 30
orientToRight  = testCase "Must be Right" $ assertEqual "Because 30 < 35"  Right $ directionRelativeTo 30 35

traceDirectionEmpty =         testCase "Trace the direction of empty array"     $ assertEqual "Must be empty" []     $ traceDirection []              []
traceDirectionSingleElement = testCase "Trace the direction of 1 element array" $ assertEqual "Must be empty" []     $ traceDirection [0]             []
traceDirectionComplex =       testCase "Trace the direction of a complex trace" $ assertEqual "" [Left, Left, Right] $ traceDirection [30, 25, 5, 10] []
