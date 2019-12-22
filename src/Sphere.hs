module Sphere
  (Sphere
  ,sphere
  ,sphereCenterEdge
  ,sphereEdgeEdge
  ,check
  ,middle
  ,sphereCenter
  ,sphereRadius) where

import Position
import Vector
import Section
import Shape
import Wrongful

data Sphere = Sphere Position Double
  deriving (Show)

sphere :: Position -> Double -> Sphere
sphere = Sphere

sphereCenterEdge :: Position -> Position -> Sphere
sphereCenterEdge c e = Sphere c $ dist $ vectorBeginEnd c e

sphereEdgeEdge :: Position -> Position -> Sphere
sphereEdgeEdge p1 p2 = Sphere c r
  where
    c = middle $ section p1 p2
    r = (/2) $ dist $ vectorBeginEnd p1 p2

instance Wrongful Sphere where
  check s
    | sphereRadius s <= 0 = Nothing
    | otherwise = Just s

instance Shape Sphere where
  middle (Sphere c _) = c

sphereCenter :: Sphere -> Position
sphereCenter (Sphere c _) = c

sphereRadius :: Sphere -> Double
sphereRadius (Sphere _ r) = r
