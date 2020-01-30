module Main where

import Value
import Point
import Vector
import Bezier
import Mesh
import Patch
import Arc
import Sphere
import Models.Bolt

main :: IO()
main = do
  exportMeshToObj [] "bolt.obj" $ removeDoubles [] 0.001 bolt
  putStrLn "Bolt"
