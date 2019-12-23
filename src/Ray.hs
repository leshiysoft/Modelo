module Ray
  (Ray
  ,ray
  ,start
  ,direction) where

import Position
import Vector

data Ray = Ray Position Vector

-- definitions

ray :: Position -> Vector -> Maybe Ray
ray p v
  | dist v <= 0 = Nothing
  | otherwise = Just $ Ray p v

-- properties

start :: Ray -> Position
start (Ray p _) = p

direction :: Ray -> Vector
direction (Ray _ v) = v
