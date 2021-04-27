module Show (pp) where

import Model
import Data.Maybe (isNothing)

pp :: Maybe Node->Int->String
pp Nothing level  = indent "|--> Nothing" level
pp (Just (Node { left = Nothing, right = Nothing, value = x })) level = indent ("|--> " ++ show x) level
pp (Just (Node { left = l, right = r, value = x }))             level = indent ("|--> " ++ show x ++ "\n" ++ pp l (level + 1) ++ "\n" ++ pp r (level + 1)) level

indent :: String->Int->String
indent s 0 = s
indent s level = "  " ++ indent s (level - 1)
