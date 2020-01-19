module Patch where

import Bezier
import Vector
import Value
import Point
import Object

data Patch = Patch
  {leftPath :: Bezier
  ,rightPath :: Bezier
  ,startTangents :: (Vector, Vector)
  ,endTangents :: (Vector, Vector)
  }

patchPoint :: Patch -> Value -> Value -> Point
patchPoint p fp rp = bezierPoint theBezier rp
  where
    theBezier = patchBezier p fp

patchBezier :: Patch -> Value -> Bezier
patchBezier p fp = theBezier
  where
    (lp0,lp1,lp2,lp3) = bezierControls $ leftPath p
    (rp0,rp1,rp2,rp3) = bezierControls $ rightPath p
    centerStart = times 0.5 (Vector lp0 + Vector rp0)
    centerEnd = times 0.5 (Vector lp3 + Vector rp3)
    leftDir = bezierDirection (leftPath p)
    rightDir = bezierDirection (rightPath p)
    zVec1 = normalize $ times 0.5 (leftDir 0 + rightDir 0)
    xVec1 = times 0.5 (Vector rp0 - Vector lp0)
    yVec1 = normalize $ cross xVec1 zVec1
    zVec2 = normalize $ times 0.5 (leftDir 1 + rightDir 1)
    xVec2 = times 0.5 (Vector rp3 - Vector lp3)
    yVec2 = normalize $ cross xVec2 zVec2
    (sTan1, sTan2) = startTangents p
    (eTan1, eTan2) = endTangents p
    sBasis = toBasis (xVec1, yVec1, zVec1)
    eBasis = toBasis (xVec2, yVec2, zVec2)
    sc1 = sBasis (Vector lp0 + sTan1 - centerStart)
    sc2 = sBasis (Vector rp0 + sTan2 - centerStart)
    ec1 = eBasis (Vector lp3 + eTan1 - centerEnd)
    ec2 = eBasis (Vector rp3 + eTan2 - centerEnd)
    pos0 = bezierPoint (leftPath p) fp
    (xp1,yp1,zp1) = vectorPoint (times (1-fp) sc1 + times fp ec1)
    (xp2,yp2,zp2) = vectorPoint (times (1-fp) sc2 + times fp ec2)
    pos1 = bezierPoint (rightPath p) fp
    leftPoint = bezierPoint (leftPath p) fp
    rightPoint = bezierPoint (rightPath p) fp
    zVec = normalize $ times 0.5 (leftDir fp + rightDir fp)
    xVec = times 0.5 (Vector rightPoint - Vector leftPoint)
    yVec = normalize $ cross xVec zVec
    centerCurrent = times 0.5 (Vector pos0 + Vector pos1)
    p1 = times xp1 xVec + times yp1 yVec + times zp1 zVec + centerCurrent
    p2 = times xp2 xVec + times yp2 yVec + times zp2 zVec + centerCurrent
    theBezier = Bezier (pos0,p1 - Vector pos0) (pos1, p2 - Vector pos1)

patchToObject :: Patch -> (Int, Int) -> Object
patchToObject p (fc, rc) = Object vtx faces []
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
