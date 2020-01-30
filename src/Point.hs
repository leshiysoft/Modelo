module Point where

import Value

type Point = (Value, Value, Value)

origin :: Point
origin  = point (0,0,0)
