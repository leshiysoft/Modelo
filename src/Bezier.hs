module Bezier where

import Point
import Vector
import Value
import Object

data Bezier = Bezier (Point, Vector) (Point, Vector)

bezierControls :: Bezier -> (Point, Point, Point, Point)
bezierControls (Bezier (pos1, v1) (pos2, v2)) = (p0,p1,p2,p3)
  where
    p0 = pos1
    p1 = move v1 pos1
    p2 = move v2 pos2
    p3 = pos2

bezierControlsAsVectors :: Bezier -> (Vector, Vector, Vector, Vector)
bezierControlsAsVectors bez = (Vector p0, Vector p1, Vector p2, Vector p3)
  where
    (p0,p1,p2,p3) = bezierControls bez

bezierPoint :: Bezier -> Value -> Point
bezierPoint bez t = vectorPoint vp
  where
    (p0,p1,p2,p3) = bezierControlsAsVectors bez
    s0 = times ((1-t)^3) p0
    s1 = times (3*t*(1-t)^2) p1
    s2 = times (3*t^2*(1-t)) p2
    s3 = times (t^3) p3
    vp = foldr (+) nullVector [s0,s1,s2,s3]

bezierDirection :: Bezier -> Value -> Vector
bezierDirection bez t = vp
  where
    (p0,p1,p2,p3) = bezierControlsAsVectors bez
    s0 = times (-3*(1-t)^2) p0
    s1 = times (3*(1-t)^2 - 6*t*(1-t)) p1
    s2 = times (6*t*(1-t) - 3*t^2) p2
    s3 = times (3*t^2) p3
    vp = foldr (+) nullVector [s0,s1,s2,s3]

bezierToObject :: Bezier -> Int -> Object
bezierToObject b i = Object vtx [] ls
    where
      toPoint ind = bezierPoint b (Value ind/fromIntegral i)
      vtx = map toPoint [0..fromIntegral i]
      ls = zip [1..fromIntegral i] [2..(fromIntegral i+1)]
