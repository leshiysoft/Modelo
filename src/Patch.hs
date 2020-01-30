module Patch where

import Bezier
import Vector
import Value
import Point
import Object
import Shape

data Patch = Patch
  {leftPath :: Bezier
  ,rightPath :: Bezier
  ,startBezier :: Bezier
  ,endBezier :: Bezier
  }

patchPoint :: Patch -> Value -> Value -> Point
patchPoint p fp rp = bezierPoint theBezier rp
  where
    theBezier = patchBezier p fp

patchBezier :: Patch -> Value -> Bezier
patchBezier p fp = theBezier
  where
    leftPoint = bezierPoint (leftPath p)
    rightPoint = bezierPoint (rightPath p)
    leftDir = bezierDirection (leftPath p)
    rightDir = bezierDirection (rightPath p)
    pathCenter x = ((xN,yN,zN),pN) where
      zN = normalize $ times 0.5 (leftDir x + rightDir x)
      xN = times 0.5 (Vector (rightPoint x) - Vector (leftPoint x))
      yN = normalize $ cross xN zN
      pN = times 0.5 (Vector (leftPoint x) + Vector (rightPoint x))
    (sysStart,centerStart) = pathCenter 0
    (sysCurrent,centerCurrent) = pathCenter fp
    (sysEnd,centerEnd) = pathCenter 1
    (_,sc1',sc2',_) = bezierControlsAsVectors $ startBezier p
    (_,ec1',ec2',_) = bezierControlsAsVectors $ endBezier p
    sc1 = toBasis sysStart (sc1' - centerStart)
    sc2 = toBasis sysStart (sc2' - centerStart)
    ec1 = toBasis sysEnd (ec1' - centerEnd)
    ec2 = toBasis sysEnd (ec2' - centerEnd)
    pos0 = leftPoint fp
    pos1 = rightPoint fp
    sc1Current = times (1-fp) sc1 + times fp ec1
    sc2Current = times (1-fp) sc2 + times fp ec2
    p1 = fromBasis sysCurrent sc1Current + centerCurrent
    p2 = fromBasis sysCurrent sc2Current + centerCurrent
    theBezier = Bezier (pos0, p1 - Vector pos0) (pos1, p2 - Vector pos1)

patchToObject :: (Int,Int) -> Patch -> Object
patchToObject (fc, rc) p = Object vtx faces []
  where
    coords = [(z,x) | x <- [0..rc], z <- [0..fc]]
    facesCoords = [(z,x) | x <- [0..rc-1], z <- [0..fc-1]]
    rDist = fromIntegral rc
    fDist = fromIntegral fc
    coordValues = map cvf coords
    cvf (z,x) = (fromIntegral z / fDist, fromIntegral x / rDist)
    vtx = map (\(fp,rp) -> bezierPoint (patchBezier p fp) rp) coordValues
    faces = map fs facesCoords
    fs (z,x) = [x*(fc+1)+z+1,x*(fc+1)+z+2,(x+1)*(fc+1)+z+2,(x+1)*(fc+1)+z+1]

instance Shape Patch where
  move v (Patch l r s e) = Patch l' r' s' e'
    where
      l' = move v l
      r' = move v r
      s' = move v s
      e' = move v e
