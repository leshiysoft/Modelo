module Plane
  (Plane
  ,plane
  ,planePosVecVec
  ,planeNormal) where

import Position
import Vector

data Plane = Plane Position Vector
  deriving (Show)

-- definitions

plane :: Position -> Vector -> Maybe Plane
plane p v
  | dist v <= 0 = Nothing
  | otherwise = Just $ Plane p v

planePosVecVec :: Position -> Vector -> Vector -> Maybe Plane
planePosVecVec p v1 v2 = plane p $ cross v1 v2

-- properties

planeNormal :: Plane -> Vector
planeNormal (Plane _ n) = n
