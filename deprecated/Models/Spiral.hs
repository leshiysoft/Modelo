module Models.Spiral where

import Spiral
import Mesh
import Bezier
import Sphere
import Point
import Vector
import Combinations

import Data.List


 --TODO: need for refactoring (move Spiral)
theSpiral1 :: Mesh
theSpiral1 = mergeMeshes $ map (bezierToMesh 100) parts
  where
    parts = map toBezier sparts
    s1 = Sphere origin 10
    v1 = right
    v2 = forward
    spiral1 = Spiral s1 v1 v2 3 5
    vs = radialVectors' up forward 8
    rs = (map ((*0.05).fromInteger) [5..50])
    sparts = zipWith4 f rs (tail rs) (cycle $ byPairs (,) vs) (map ((*0.25).fromInteger) [0..10])
    f r nr (v1,v2) y = Spiral (Sphere (0,y,0) r) v1 v2 0.25 nr
