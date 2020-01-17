module Main where

import Position
import Vector
import Section
import Sphere
import Plane
import Extra
import Ray
import Line
import Object
import SphereSurface
import Surface

import Data.Maybe

main :: IO ()
main = do
  segment <- return $ makeSegment
  writeFile "f1.obj" $ makeObj $ mesh 40 segment

makeSegment :: Segment Sphere
makeSegment = Segment s curves
  where
    (Just s) = sphere origin 1
    [p1,p2,p3,p4] = [fromJust $ spherePoint s $ vector p | p <- [(1,1,1),(1,-1,1),(-1,-1,1),(-1,1,1)]]
    curves = [CurvePP p1 p2 s, CurvePP p2 p3 s, CurvePP p3 p4 s, CurvePP p4 p1 s]



-- main = do
--   (Just s) <- return $ sphere origin 1
--   ss <- return $ SphereSurfaceTri s (Vector (1,0,0)) (Vector (0,1,0)) (Vector (0,0,1))
--   mss <- return $ quad ss
--   writeFile "f1.obj" $ makeObj $ mergeObjects $ map toObject mss
