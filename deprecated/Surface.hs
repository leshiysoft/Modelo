{-# LANGUAGE GADTs #-}
module Surface where

import Position
import Sphere
import Vector
import Object

data CurveOnSurf surf where
  CurvePP :: Position -> Position -> surf -> CurveOnSurf surf

-- properties

surfOfCurve :: CurveOnSurf surf -> surf
surfOfCurve (CurvePP _ _ s) = s

scopeOfCurve :: CurveOnSurf surf -> (Position, Position)
scopeOfCurve (CurvePP s e _) = (s, e)



data Segment surf = Segment surf [CurveOnSurf surf]

mesh :: Surface surf => Int -> Segment surf -> Object
mesh n (Segment surf curves)
  | length curves == 4 = Object meshPoints faces
    where
      curveLeft = curves !! 0
      curveRight = reverseCurve (curves !! 2)
      ps = [fromIntegral x / fromIntegral n | x <- [1..(n-1)]]
      fronts = [path (curves !! 3) p | p <- reverse ps]
      backs = [path (curves !! 1) p | p <- ps]
      otherCurves = zipWith (\f b -> CurvePP f b surf) fronts backs
      allCurves = [curveLeft] ++ otherCurves ++ [curveRight]
      meshPoints = [path c p | c <- allCurves, p <- [0] ++ ps ++ [1]]
      facesCoords = [ (x,y) | x <- [0..(n-1)], y <- [0..(n-1)]]
      faces = [ [x+1+y*(n+1),x+2+y*(n+1),x+2+(y+1)*(n+1),x+1+(y+1)*(n+1)] | (x,y) <- facesCoords]
  -- | length curves == 3 = if (mod n 2 == 0) then res else []
  --   where
  --     curveLeft = reverseCurve (curves !! 2)
  --     curveRight = curves !! 0
  --     curveBottom = curves !! 1
  --     (topPoint, _) = scopeOfCurve curveRight
  --     bottomPoint = path curveBottom 0.5
  --     middlePoint = path (CurvePP bottomPoint topPoint surf) 0.5
  --     rightPoint = path curveRight 0.5
  --     leftPoint = path curveLeft 0.5
      




class Surface surf where
  path :: CurveOnSurf surf -> (Double -> Position)
  reverseCurve :: CurveOnSurf surf -> CurveOnSurf surf
  reverseCurve (CurvePP p1 p2 surf) = CurvePP p2 p1 surf

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
