module Models.Spiral where

import Spiral
import Mesh
import Curve
import Point
import Vector


 --TODO: need for refactoring (move Spiral)
theSpiral1 :: Mesh
theSpiral1 = curveToMesh 100 $ toCurve sprl
  where
    sprl = Spiral origin (times 5 up) (times 3 right) 5 1
