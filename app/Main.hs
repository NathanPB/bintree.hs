module Main where

import Model
import Navigation
import Transformation
import Assemble
import Prelude hiding (Left, Right)

pp :: Maybe Node->Int->String
pp Nothing level  = indent "|--> Nothing" level
pp (Just (Node { left = Nothing, right = Nothing, value = x })) level = indent ("|--> " ++ show x) level
pp (Just (Node { left = l, right = r, value = x }))             level = indent ("|--> " ++ show x ++ "\n" ++ pp l (level + 1) ++ "\n" ++ pp r (level + 1)) level

indent :: String->Int->String
indent s 0 = s
indent s level = "  " ++ indent s (level - 1)

main :: IO ()
main = do
  let tree = treeFrom [30, 25, 35, 50, 30, 5, 100, 60, 65, 10]
  putStr $ pp (Just tree) 0
  putStr "\n-----\n"
  putStr $ pp (deleteNode tree []) 0
  --print $ find (Just tree) 60 []
  -- print $ depth tree 0
  -- print $ map (\x -> case x of Nothing -> Nothing; Just y -> Just $ value y) $ navigate [tree] [Left, Left, Right, Left]
  -- putStr $ pp $ append Just (append Just (append Just (append Just (append Nothing 2) 3) 4) 1) 3
