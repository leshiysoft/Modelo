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

main :: IO ()
main = do
  (Just s) <- return $ sphere origin 1
  ss <- return $ SphereSurfaceTri s (Vector (1,0,0)) (Vector (0,1,0)) (Vector (0,0,1))
  mss <- return $ quad ss
  writeFile "f1.obj" $ makeObj $ mergeObjects $ map toObject mss
  -- putStrLn $ makeObj $ mergeObjects $ map toObject mss
