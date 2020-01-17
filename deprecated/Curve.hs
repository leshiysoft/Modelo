module Curve where

import Vector
import Sphere
import Position

data Curve = Arc Sphere Vector Vector
  deriving Show

curveBegin :: Curve -> Position
curveBegin (Arc s v1 _) = let (Just p) = spherePoint s v1 in p

curveEnd :: Curve -> Position
curveEnd (Arc s _ v2) = let (Just p) = spherePoint s v2 in p

curvePoint :: Curve -> Double -> Position
curvePoint (Arc sph v1 v2) p = move rvec center
  where
    center = sphereCenter sph
    (Just a) = normalize v1
    (Just b) = normalize v2
    (Just c) = normalize $ cross (cross a b) a
    alpha = acos (dot a b) * p
    rvec = plus (times (sin alpha) c) (times (cos alpha) a)

data Bezier = Bezier (Position, Vector) (Position, Vector)

bezierPoint :: Bezier -> Double -> Position
bezierPoint (Bezier (pos1, v1) (pos2, v2)) t = vectorPosition vp
  where
    p0 = vector pos1
    p1 = vector $ move v1 pos1
    p2 = vector $ move v2 pos2
    p3 = vector pos2
    s0 = times ((1-t)^3) p0
    s1 = times (3*t*(1-t)^2) p1
    s2 = times (3*t^2*(1-t)) p2
    s3 = times (t^3) p3
    vp = foldr plus nullVector [s0,s1,s2,s3]

curveToBezier :: Curve -> Bezier
curveToBezier (Arc sph v1 v2) = Bezier () ()
  where
    center = sphereCenter sph
