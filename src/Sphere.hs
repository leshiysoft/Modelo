module Sphere
  (Sphere
  ,sphere
  ,sphereCenterEdge
  ,sphereEdgeEdge
  ,sphereCenter
  ,sphereRadius
  ,middle) where

import Position
import Vector
import Section
import Shape

data Sphere = Sphere Position Double
  deriving (Show)

-- definitions

sphere :: Position -> Double -> Maybe Sphere
sphere p r
  | r <= 0 = Nothing
  | otherwise = Just $ Sphere p r

sphereCenterEdge :: Position -> Position -> Maybe Sphere
sphereCenterEdge c e = sphere c $ dist $ vectorBeginEnd c e

sphereEdgeEdge :: Position -> Position -> Maybe Sphere
sphereEdgeEdge p1 p2 = sphere c r
  where
    c = middle $ section p1 p2
    r = (/2) $ dist $ vectorBeginEnd p1 p2

-- properties

sphereCenter :: Sphere -> Position
sphereCenter (Sphere c _) = c

sphereRadius :: Sphere -> Double
sphereRadius (Sphere _ r) = r

-- instances

instance Shape Sphere where
  middle (Sphere c _) = c
