module Arc where

import Vector
import Sphere
import Point
import Curve
import Combinations
import Shape

data Arc = Arc Sphere Vector Vector
  deriving (Eq, Show)

arcStart :: Arc -> Point
arcStart (Arc s v1 _) = spherePoint s v1

arcEnd :: Arc -> Point
arcEnd (Arc s _ v2) = spherePoint s v2

instance Spline Arc where
  toCurve (Arc s v1 v2) = CurveDir cf df
    where
      nv1 = normalize v1
      nv2 = normalize v2
      zv = cross v1 v2
      yv = normalize $ cross zv v1
      alpha = acos $ dot nv1 nv2
      pv v = let a = alpha * v in times (cos a) nv1 + times (sin a) yv
      cf v = move (normalizeTo (sphereRadius s) $ pv v) $ sphereCenter s
      df v = cross zv (pv v)

radialArcs :: Sphere -> Vector -> Vector -> Int -> [Arc]
radialArcs s n r i = byPairs (Arc s) (radialVectors' n r i)

instance Shape Arc where
  move v (Arc s a b) = Arc (move v s) a b
