module Main where

import Model
import Show
import Navigation
import Transformation
import Prelude hiding (Left, Right)

main :: IO ()
main = do
  let tree = (foldl (\tree val -> Just $ append tree val) Nothing [30, 25, 35, 50, 30, 5, 100, 60, 65, 10])
  -- print $ find tree 63 []
  -- putStr $ pp tree 0
  -- print $ depth tree 0
  print $ map (\x -> case x of Nothing -> Nothing; Just y -> Just $ value y) $ navigate [tree] [Left, Left, Right, Left]
  -- putStr $ pp $ append Just (append Just (append Just (append Just (append Nothing 2) 3) 4) 1) 3
