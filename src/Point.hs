module Point where

import Value

type Point = (Value, Value, Value)

point :: (Double, Double, Double) -> Point
point (x, y, z) = (Value x, Value y, Value z)

origin :: Point
origin  = point (0,0,0)
