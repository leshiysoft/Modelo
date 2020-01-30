module Models.Bolt where

import Patch
import Sphere
import Vector
import Section
import Point
import Bezier
import Arc
import Combinations
import Data.List

bolt :: [Patch]
bolt = pat1 : pat2 : pat3 : pat4 : patches
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
    se2 = map toBezier $ radialArcs (Sphere (0,screwHeight,0) screwRadius) up forward 6     --TODO: методом moveUp класса Shape
    se3 = map toBezier $ radialSections (Sphere (0,screwHeight,0) headRadius) up forward 6
    se4 = map toBezier $ radialSections (Sphere (0,totalHeight,0) headRadius) up forward 6   --TODO: методом moveUp класса Shape
    plrs = concat $ map (byPairs Patch) paths
    beziers = concat $ byPairs zip [se1, se2, se3, se4]
    patches = zipWith ($) (map uncurry plrs) beziers
    [a,b,c,d,e,f,_] = ps4
    pat1 = faceToPatch a b c d
    pat2 = faceToPatch d e f a    --TODO: патчинг полигонов
    [a1,a2,a3,a4,a5,a6] = se1
    [ap,_,_,dp,_,_,_] = ps1
    pat3 = Patch (invertBezier a1) a3 a2 (sectionBezier ap dp)
    pat4 = Patch (invertBezier a4) a6 a5 (sectionBezier dp ap) --TODO: патчинг окружностей

faceToPatch :: Point -> Point -> Point -> Point -> Patch
faceToPatch t1 t2 t3 t4 = Patch b1 b2 b3 b4
  where
    b1 = sectionBezier t1 t2
    b2 = sectionBezier t4 t3
    b3 = sectionBezier t1 t4
    b4 = sectionBezier t2 t3
