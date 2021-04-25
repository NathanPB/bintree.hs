module OrientationSpec (orientationSpecs) where

import Model
import Orientations
import Prelude hiding (Left, Right)
import Test.Tasty
import Test.Tasty.HUnit

leftNode = Node { value = 25, left = Nothing, right = Nothing }
rightNode = Node { value = 35, left = Nothing, right = Nothing }
parentNode = Node { value = 30, left = Just leftNode, right = Just rightNode }

orientationSpecs = [ orientToLeft, orientIfEquals, orientToRight, orientToLeft', orientIfEquals', orientToRight' ]

orientToLeft = testCase "Must be Left" $ assertEqual "Because 30 > 25" Left $ directionRelativeTo parentNode leftNode

orientIfEquals = testCase "Must be Right" $ assertEqual "Because 30 == 30" Right $ directionRelativeTo parentNode parentNode

orientToRight = testCase "Must be Right" $ assertEqual "Because 30 < 35" Right $ directionRelativeTo parentNode rightNode



orientToLeft' = testCase "Must be Left" $ assertEqual "Because 30 > 25" Left $ directionRelativeTo' 30 25

orientIfEquals' = testCase "Must be Right" $ assertEqual "Because 30 == 30" Right $ directionRelativeTo' 30 30

orientToRight' = testCase "Must be Right" $ assertEqual "Because 30 < 35" Right $ directionRelativeTo' 30 35
