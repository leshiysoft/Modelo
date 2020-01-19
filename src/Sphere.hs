module Sphere where

import Point
import Value
import Vector

data Sphere = Sphere
  {sphereCenter :: Point
  ,sphereRadius :: Value
  }
  deriving (Eq, Show)

sphereCenterEdge :: Point -> Point -> Sphere
sphereCenterEdge c e = Sphere c $ dist $ vectorBeginEnd c e

sphereEdgeEdge :: Point -> Point -> Sphere
sphereEdgeEdge p1 p2 = Sphere c r
  where
    c = vectorPoint $ times 0.5 (Vector p2 + Vector p1)
    r = (/2) $ dist $ vectorBeginEnd p1 p2

spherePoint :: Sphere -> Vector -> Point
spherePoint (Sphere c r) v = vectorPoint (Vector c + pv)
  where
    pv = normalizeTo r v
