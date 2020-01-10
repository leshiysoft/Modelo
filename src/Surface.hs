{-# LANGUAGE GADTs #-}
module Surface where

import Position
import Sphere
import Vector

data CurveOnSurf surf where
  CurvePP :: Position -> Position -> surf -> CurveOnSurf surf

-- properties

surfOf :: CurveOnSurf surf -> surf
surfOf (CurvePP _ _ s) = s

scopeOf :: CurveOnSurf surf -> (Position, Position)
scopeOf (CurvePP s e _) = (s, e)



data Segment surf = Segment surf [(Position, CurveOnSurf surf)]




class Surface surf where
  path :: CurveOnSurf surf -> (Double -> Position)

-- instances

instance Surface Sphere where
  path (CurvePP p1 p2 sph) p = move rvec center
    where
      center = sphereCenter sph
      (Just a) = normalize $ vectorBeginEnd center p1
      (Just b) = normalize $ vectorBeginEnd center p2
      (Just c) = normalize $ cross (cross a b) a
      alpha = acos (dot a b) * p
      rvec = plus (times (sin alpha) c) (times (cos alpha) a)
