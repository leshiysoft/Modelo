module Sphere where

import Canonical
import Position
import Vector
import Section
import Shape
import Wrongful

data Sphere = Sphere Position Double
  | SphereCenterEdge Position Position
  | SphereDiameter Position Position
  deriving (Show)

instance Canonical Sphere where
  canon s@(Sphere _ _) = s
  canon (SphereCenterEdge c e) =
    Sphere c $ dist $ Vector2Dots c e
  canon (SphereDiameter p1 p2) = Sphere c r
    where
      c = middle $ Section p1 p2
      r = (/2) $ dist $ Vector2Dots p1 p2

instance Wrongful Sphere where
  check s
    | sphereRadius s <= 0 = Nothing
    | otherwise = Just s

sphereCenter :: Sphere -> Position
sphereCenter s = let (Sphere c _) = canon s in c

sphereRadius :: Sphere -> Double
sphereRadius s = let (Sphere _ r) = canon s in r
