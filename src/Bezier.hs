module Bezier where

import Point
import Vector
import Value

data Bezier = Bezier (Point, Vector) (Point, Vector)

bezierPoint :: Bezier -> Double -> Point
bezierPoint (Bezier (pos1, v1) (pos2, v2)) tp = vectorPoint vp
  where
    t = Value tp
    p0 = Vector pos1
    p1 = Vector $ move v1 pos1
    p2 = Vector $ move v2 pos2
    p3 = Vector pos2
    s0 = times ((1-t)^3) p0
    s1 = times (3*t*(1-t)^2) p1
    s2 = times (3*t^2*(1-t)) p2
    s3 = times (t^3) p3
    vp = foldr (+) nullVector [s0,s1,s2,s3]
