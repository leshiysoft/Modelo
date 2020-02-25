module Patch where

import Curve
import Vector
import Value
import Point
import Mesh
import Shape

data Patch = Patch
  {leftPath :: Curve
  ,rightPath :: Curve
  ,startCurve :: Curve
  ,endCurve :: Curve
  }

instance Shape Patch where
  move v (Patch l r s e) = Patch l' r' s' e'
    where
      l' = move v l
      r' = move v r
      s' = move v s
      e' = move v e

patchPoint :: Patch -> Value -> Value -> Point
patchPoint p fp rp = vectorPoint currentP
  where
    leftPoint = curveFunction (leftPath p)
    rightPoint = curveFunction (rightPath p)
    leftDir = curveDirection (leftPath p)
    rightDir = curveDirection (rightPath p)
    system x = ((xN,yN,zN),pN) where
      zN = normalize (leftDir x + rightDir x)
      xN = times 0.5 (Vector (rightPoint x) - Vector (leftPoint x))
      yN = normalize $ cross xN zN
      pN = times 0.5 (Vector (leftPoint x) + Vector (rightPoint x))
    (sysStart,centerStart) = system 0
    (sysCurrent,centerCurrent) = system fp
    (sysEnd,centerEnd) = system 1
    startP = Vector $ curveFunction (startCurve p) rp
    endP = Vector $ curveFunction (endCurve p) rp
    startP' = toBasis sysStart (startP - centerStart)
    endP' = toBasis sysEnd (endP - centerEnd)
    currentP' = times (1-fp) startP' + times fp endP'
    currentP = fromBasis sysCurrent currentP' + centerCurrent

patchToMesh :: (Int,Int) -> Patch -> Mesh
patchToMesh (fc, rc) p = Mesh vtx faces []
  where
    coords = [(z,x) | x <- [0..rc], z <- [0..fc]]
    facesCoords = [(z,x) | x <- [0..rc-1], z <- [0..fc-1]]
    rDist = fromIntegral rc
    fDist = fromIntegral fc
    coordValues = map cvf coords
    cvf (z,x) = (fromIntegral z / fDist, fromIntegral x / rDist)
    vtx = map (\(fp,rp) -> patchPoint p fp rp) coordValues
    faces = map fs facesCoords
    fs (z,x) = [x*(fc+1)+z+1,x*(fc+1)+z+2,(x+1)*(fc+1)+z+2,(x+1)*(fc+1)+z+1]
