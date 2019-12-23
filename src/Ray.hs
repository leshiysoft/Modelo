module Ray
  (Ray
  ,ray
  ,rayStart
  ,rayDirection) where

import Position
import Vector

data Ray = Ray Position Vector

-- definitions

ray :: Position -> Vector -> Maybe Ray
ray p v
  | dist v <= 0 = Nothing
  | otherwise = Just $ Ray p v

-- properties

rayStart :: Ray -> Position
rayStart (Ray p _) = p

rayDirection :: Ray -> Vector
rayDirection (Ray _ v) = v
