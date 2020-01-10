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
  discrete :: Int -> (CurveOnSurf surf) -> [Position]

-- instances

instance Surface Sphere where
  discrete 0 _ = []
  discrete 1 (CurvePP p1 p2 sph) = [p]
    where
      center = sphereCenter sph
      v1 = vectorBeginEnd center p1
      v2 = vectorBeginEnd center p2
      (Just p) = spherePoint sph $ plus v1 v2
