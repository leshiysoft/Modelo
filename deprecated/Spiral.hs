module Spiral where

import Vector
import Sphere
import Value
import Point
import Bezier
import Arc
import Shape

-- Sphere Vector Vector is Arc
-- Value Value are step and new radius
data Spiral = Spiral Sphere Vector Vector Value Value
  deriving(Eq, Show)

spiralStart :: Spiral -> Point
spiralStart (Spiral s v1 _ _ _) = spherePoint s v1

spiralEnd :: Spiral -> Point
spiralEnd spr@(Spiral s _ v2 _ r) = spherePoint s2 v2
  where
    s2 = Sphere (move (spiralOffset spr) $ sphereCenter s) r

spiralStepDirection :: Spiral -> Vector
spiralStepDirection (Spiral _ v1 v2 _ _) = cross v1 v2

spiralOffset :: Spiral -> Vector
spiralOffset spr@(Spiral _ _ _ step _) = normalizeTo step $ spiralStepDirection spr

--TODO: need for refactoring
--TODO: wrong with different radiuses
instance Spline Spiral where
  toBezier spr@(Spiral s v1 v2 step r) = Bezier (p1, c1) (p2, c2)
    where
      p1 = spiralStart spr
      p2 = spiralEnd spr
      arc1 = Arc s v1 v2
      sprOffset = spiralOffset spr
      sphere2 = Sphere (sphereCenter $ move sprOffset s) r
      arc2 = Arc sphere2 v1 v2
      Bezier (_, c1') _ = toBezier arc1
      Bezier _ (_, c2') = toBezier arc2
      c1 = c1' + (times (1/3) sprOffset)
      c2 = c2' - (times (1/3) sprOffset)
