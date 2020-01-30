module Models.Bolt where

import Patch
import Sphere
import Vector
import Section
import Point
import Bezier
import Arc
import Combinations
import Shape
import Mesh
import Data.List

bolt :: Mesh
bolt = mergeMeshes $ map (patchToMesh (4,4)) $ pat1:pat2:pat3:pat4:patches
  where
    headHeight = 1
    headRadius = 3
    screwHeight = 10
    screwRadius = 1
    totalHeight = screwHeight + headHeight
    vs = radialVectors' up forward 6
    ps1 = map (vectorPoint . normalizeTo screwRadius) vs
    ps2 = map (moveUp screwHeight) ps1
    ps3 = map (moveUp screwHeight . vectorPoint . normalizeTo headRadius) vs
    ps4 = map (moveUp headHeight) ps3
    paths = byPairs (zipWith sectionBezier) [ps1, ps2, ps3, ps4]
    se1 = map toBezier $ radialArcs (Sphere origin screwRadius) up forward 6
    se2 = map (moveUp screwHeight) se1
    se3 = map toBezier $ radialSections (Sphere (0,screwHeight,0) headRadius) up forward 6
    se4 = map (moveUp headHeight) se3
    plrs = concat $ map (byPairs Patch) paths
    beziers = concat $ byPairs zip [se1, se2, se3, se4]
    patches = zipWith ($) (map uncurry plrs) beziers
    [a,b,c,d,e,f,_] = ps4
    pat1 = faceToPatch d c b a
    pat2 = faceToPatch a f e d    --TODO: патчинг полигонов
    [a1,a2,a3,a4,a5,a6] = se1
    [ap,_,_,dp,_,_,_] = ps1
    pat3 = Patch a1 (invertBezier a3) (sectionBezier ap dp) a2
    pat4 = Patch a4 (invertBezier a6) (sectionBezier dp ap) a5 --TODO: патчинг окружностей

faceToPatch :: Point -> Point -> Point -> Point -> Patch
faceToPatch t1 t2 t3 t4 = Patch b1 b2 b3 b4
  where
    b1 = sectionBezier t1 t2
    b2 = sectionBezier t4 t3
    b3 = sectionBezier t1 t4
    b4 = sectionBezier t2 t3
