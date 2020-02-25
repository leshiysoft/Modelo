module Spiral where

import Vector
import Value
import Point
import Curve
import Shape

-- Аргументы: центр первого витка, ось (длина), радиал (начальный радиус),
-- количество витков, конечный радиус
data Spiral = Spiral
  { spiralCenter :: Point -- центр первого витка
  , spiralDirection :: Vector -- направление спирали и ее длина
  , spiralRadial :: Vector -- радиальный вектор (радиус первого витка)
  , spiralTurns :: Value -- количество витков
  , spiralRadius2 :: Value -- радиус последнего витка
  }
  deriving(Eq, Show)

spiralStart :: Spiral -> Point
spiralStart (Spiral c _ r _ _) = move r c

-- TODO: учитывать количество витков
spiralEnd :: Spiral -> Point
spiralEnd spr@(Spiral c d r m nr) = move (d + normalizeTo nr r) c

-- TODO: аналитическая формула направляющего вектора
instance Spline Spiral where
  toCurve (Spiral c d r t r2) = Curve cf
    where
      y = normalizeTo (dist r) $ cross r d
      dirOffset v = times v d
      angle v = 2*pi*t*v
      angleOffset v = times (cos $ angle v) r + times (sin $ angle v) y
      radialOffset v = normalizeTo ((1-v)*(dist r) + v*r2) $ angleOffset v
      cf v = move (dirOffset v + radialOffset v) c

instance Shape Spiral where
  move v (Spiral c d r t r2) = Spiral (move v c) d r t r2
