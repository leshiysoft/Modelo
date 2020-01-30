module Sphere where

import Point
import Value
import Vector
import Section
import Combinations

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

radialVectors :: Vector -> Vector -> Int -> [Vector]
radialVectors n r i = map vector angles
  where
    zVector = normalize $ cross r n
    refVector = normalize $ cross n zVector
    angles = [ 2*pi*j/(fromIntegral i)  | j <- map fromIntegral [0..i-1]]
    vector a = times (sin a) refVector  + times (cos a) zVector

radialVectors' :: Vector -> Vector -> Int -> [Vector]
radialVectors' n r i = vs ++ [head vs]
  where
    vs = radialVectors n r i

radialPoints :: Sphere -> Vector -> Vector -> Int -> [Point]
radialPoints s n r i = map (spherePoint s) $ radialVectors n r i

radialPoints' :: Sphere -> Vector -> Vector -> Int -> [Point]
radialPoints' s n r i = map (spherePoint s) $ radialVectors' n r i

radialSections :: Sphere -> Vector -> Vector -> Int -> [Section]
radialSections s n r i = byPairs Section (radialPoints' s n r i)
