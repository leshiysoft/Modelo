module Arc where

import Vector
import Sphere
import Point
import Bezier

data Arc = Arc Sphere Vector Vector
  deriving (Eq, Show)

arcStart :: Arc -> Point
arcStart (Arc s v1 _) = spherePoint s v1

arcEnd :: Arc -> Point
arcEnd (Arc s _ v2) = spherePoint s v2

arcToBezier :: Arc -> Bezier
arcToBezier a@(Arc s v1 v2) = Bezier (p1, c1) (p2, c2)
  where
    p1 = arcStart a
    p2 = arcEnd a
    a1 = normalizeTo (sphereRadius s) v1
    a2 = normalizeTo (sphereRadius s) v2
    n = normalizeTo (sphereRadius s) (v1 + v2)
    m = times 0.5 (times (8/3) n - times (1/3) (a1 + a2))
    nn = normalize n
    z = cross v1 v2
    cn1 = normalize $ cross z v1
    cn2 = normalize $ cross v2 z
    k = dot nn cn1
    theDist = dist (m - times 0.5 (a1 + a2)) / k
    c1 = times theDist cn1
    c2 = times theDist cn2
