module Polygon where

import Sphere
import Vector
import Value
import Point

data Polygon = Polygon
  {polygonExcircle :: Sphere
  ,polygonNormal :: Vector
  ,polygonRef :: Vector
  ,polygonSides :: Int
  }
  deriving (Show, Eq)

polygonVectors :: Polygon -> [Vector]
polygonVectors (Polygon _ n r i) = map vector angles
  where
    zVector = normalize $ cross r n
    refVector = normalize $ cross n zVector
    angles = [ 2*pi*j/(fromIntegral i)  | j <- map fromIntegral [0..i-1]]
    vector a = times (sin a) refVector  + times (cos a) zVector

polygonVertexes :: Polygon -> [Point]
polygonVertexes p = map f $ polygonVectors p
  where
    f = spherePoint (polygonExcircle p)
