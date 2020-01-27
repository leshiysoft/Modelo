module Models.Bolt where

import Patch
import Sphere
import Vector
import Section
import Point
import Bezier
import Arc
import Data.List

bolt :: [Patch]
bolt = [pat1, pat2, pat3, pat4] ++ headSides ++ headBottoms ++ screwSides
  where
    headHeight = 1
    headRadius = 3
    screwHeight = 10
    screwRadius = 1
    s1 = Sphere (0,0,0) screwRadius
    s2 = Sphere (0,screwHeight,0) screwRadius
    s3 = Sphere (0,screwHeight,0) headRadius
    s4 = Sphere (0,screwHeight + headHeight,0) headRadius
    sin60 = sqrt(3)/2
    va = Vector (0, 0, 1)
    vb = Vector (sin60, 0, 0.5)
    vc = Vector (sin60, 0, -0.5)
    vd = Vector (0, 0, -1)
    ve = Vector (-sin60, 0, -0.5)
    vf = Vector (-sin60, 0, 0.5)
    vec6 = [va,vb,vc,vd,ve,vf]
    [a,b,c,d,e,f] = map (spherePoint s4) vec6
    pat1 = faceToPatch (a,d,e,f)
    pat2 = faceToPatch (d,a,b,c)
    headVector = Vector (0, -headHeight, 0)
    [a1,b1,c1,d1,e1,f1] = map (move headVector) [a,b,c,d,e,f]
    headSides = map faceToPatch
      [(a,b,b1,a1),(b,c,c1,b1),(c,d,d1,c1),(d,e,e1,d1),(e,f,f1,e1),(f,a,a1,f1)]
    [a2,b2,c2,d2,e2,f2] = map (spherePoint s2) vec6
    arcs2 = map (\(x,y) -> Arc s2 x y) $ pairs vec6
    sideSecs = map (uncurry Section) $ pairs [a1,b1,c1,d1,e1,f1]
    radSecs = map (uncurry Section) $ zip [a1,b1,c1,d1,e1,f1] [a2,b2,c2,d2,e2,f2]
    btp = \(a,b,c,d) -> Patch (toBezier a) (toBezier b)
      (toBezier c) (toBezier d)
    headBottoms = map btp $ zip4 (cycle radSecs) (tail $ cycle radSecs)
      sideSecs arcs2
    [a3,b3,c3,d3,e3,f3] = map (spherePoint s1) vec6
    vertSecs = map (uncurry Section) $ zip [a2,b2,c2,d2,e2,f2] [a3,b3,c3,d3,e3,f3]
    arcs1 = map (\(x,y) -> Arc s1 x y) $ pairs vec6
    btp2 = \(a,b,c,d) -> Patch (toBezier a) (toBezier b)
      (toBezier c) (toBezier d)
    screwSides = map btp2 $ zip4 (cycle vertSecs) (tail $ cycle vertSecs)
      arcs2 arcs1
    pat3 = Patch (arcBezier s1 vf va) (arcBezier s1 ve vd)
      (arcBezier s1 vf ve) (sectionBezier a3 d3)
    pat4 = Patch (arcBezier s1 vc vd) (arcBezier s1 vb va)
      (arcBezier s1 vc vb) (sectionBezier d3 a3)

pairs :: [a] -> [(a,a)]
pairs l = zip l $ tail $ cycle l

faceToPatch :: (Point, Point, Point, Point) -> Patch
faceToPatch (t1,t2,t3,t4) = Patch b1 b2 b3 b4
  where
    b1 = sectionBezier t1 t2
    b2 = sectionBezier t4 t3
    b3 = sectionBezier t1 t4
    b4 = sectionBezier t2 t3
