module Patch where

import Bezier
import Vector
import Value
import Point
import Object

data Patch = Patch
  {leftPath :: Bezier
  ,rightPath :: Bezier
  ,startBezier :: Bezier
  ,endBezier :: Bezier
  }

-- beziersToPatch :: Bezier -> Bezier -> Bezier -> Bezier -> Patch
-- beziersToPatch lb rb sb eb = Patch lb rb (st1,st2) (et1,et2)
--   where
--     Bezier (_,st1) (_,st2) = sb
--     Bezier (_,et1) (_,et2) = eb

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
    --(lp0,lp1,lp2,lp3) = bezierControls $ leftPath p
    --(rp0,rp1,rp2,rp3) = bezierControls $ rightPath p
    (sysStart,centerStart) = pathCenter 0
    (sysCurrent,centerCurrent) = pathCenter fp
    (sysEnd,centerEnd) = pathCenter 1
    (_,sc1',sc2',_) = bezierControlsAsVectors $ startBezier p
    (_,ec1',ec2',_) = bezierControlsAsVectors $ endBezier p
    --centerStart = times 0.5 (Vector lp0 + Vector rp0)
    --centerEnd = times 0.5 (Vector lp3 + Vector rp3)
    -- zVec1 = normalize $ times 0.5 (leftDir 0 + rightDir 0)
    -- xVec1 = times 0.5 (Vector rp0 - Vector lp0)
    -- yVec1 = normalize $ cross xVec1 zVec1
    -- zVec2 = normalize $ times 0.5 (leftDir 1 + rightDir 1)
    -- xVec2 = times 0.5 (Vector rp3 - Vector lp3)
    -- yVec2 = normalize $ cross xVec2 zVec2
    --(sTan1, sTan2) = bezierTangents $ startBezier p
    --(eTan1, eTan2) = bezierTangents $ endBezier p
    sc1 = toBasis sysStart (sc1' - centerStart)
    sc2 = toBasis sysStart (sc2' - centerStart)
    ec1 = toBasis sysEnd (ec1' - centerEnd)
    ec2 = toBasis sysEnd (ec2' - centerEnd)
    pos0 = leftPoint fp
    pos1 = rightPoint fp
    sc1Current = times (1-fp) sc1 + times fp ec1
    sc2Current = times (1-fp) sc2 + times fp ec2
    -- zVec = normalize $ times 0.5 (leftDir fp + rightDir fp)
    -- xVec = times 0.5 (Vector pos1 - Vector pos0)
    -- yVec = normalize $ cross xVec zVec
    -- centerCurrent = times 0.5 (Vector pos0 + Vector pos1)
    p1 = fromBasis sysCurrent sc1Current + centerCurrent
    p2 = fromBasis sysCurrent sc2Current + centerCurrent
    -- p1 = times xp1 xVec + times yp1 yVec + times zp1 zVec + centerCurrent
    -- p2 = times xp2 xVec + times yp2 yVec + times zp2 zVec + centerCurrent
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
