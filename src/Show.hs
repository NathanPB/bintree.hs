module Show (pp) where

import Model
import Data.Maybe (isNothing)

pp :: Maybe Node->Int->String
pp node level = do
  case node of
    Nothing -> indent "|--> Nothing" level
    Just x -> do
      if isLeaf x then
        indent ("|--> " ++ show (value x)) level
      else
        indent ("|--> " ++ show (value x) ++ "\n" ++ pp (left x) (level + 1) ++ "\n" ++ pp (right x) (level + 1)) level

indent :: String->Int->String
indent s 0 = s
indent s level = "  " ++ indent s (level - 1)

isLeaf :: Node->Bool
isLeaf x = isNothing (left x) && isNothing (right x)
