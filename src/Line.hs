module Line
  (Line
  ,line
  ,lineNormal) where

import Position
import Vector

data Line = Line Position Vector

-- definitions

line :: Position -> Vector -> Maybe Line
line p v
  | dist v <= 0 = Nothing
  | otherwise = Just $ Line p v

-- properties

lineNormal :: Line -> Vector
lineNormal (Line _ n) = n
