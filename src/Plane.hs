module Plane
  (Plane
  ,plane
  ,planePosVecVec
  ,check
  ,planeNormal) where

import Position
import Vector
import Wrongful

data Plane = Plane Position Vector
  deriving (Show)

plane :: Position -> Vector -> Plane
plane = Plane

planePosVecVec :: Position -> Vector -> Vector -> Plane
planePosVecVec p v1 v2 = Plane p $ cross v1 v2

instance Wrongful Plane where
  check p
    | (dist $ planeNormal p) <= 0 = Nothing
    | otherwise = Just p

planeNormal :: Plane -> Vector
planeNormal (Plane _ n) = n
