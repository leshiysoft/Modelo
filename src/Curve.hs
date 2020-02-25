module Curve where

import Value
import Point
import Vector
import Shape
import Mesh

data Curve = Curve (Value -> Point)
  | CurveDir (Value -> Point) (Value -> Vector)

curveFunction :: Curve -> (Value -> Point)
curveFunction (Curve cf) = cf
curveFunction (CurveDir cf _) = cf

curveDirection :: Curve -> (Value -> Vector)
curveDirection (Curve cf) = \v -> vectorBeginEnd (cf v) (cf (v + 0.001))
curveDirection (CurveDir _ cd) = cd

invertCurve :: Curve -> Curve
invertCurve (Curve cf) = Curve (\v -> cf (1-v))
invertCurve (CurveDir cf df) = CurveDir cf' df'
  where
    cf' v = cf (1-v)
    df' v = times (-1) $ df (1-v)

curveToMesh :: Int -> Curve -> Mesh
curveToMesh i c = Mesh vtx [] ls
    where
      toPoint ind = curveFunction c (Value ind/fromIntegral i)
      vtx = map toPoint [0..fromIntegral i]
      ls = zip [1..fromIntegral i] [2..(fromIntegral i+1)]

class Spline s where
  toCurve :: s -> Curve

instance Shape Curve where
  move v (Curve cf) = Curve ((move v).cf)
  move v (CurveDir cf df) = CurveDir ((move v).cf) df
