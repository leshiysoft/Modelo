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
import Models.Spiral

main :: IO()
main = do
  exportMeshToObj [] "bolt.obj" $ removeDoubles [] 0.001 bolt
  putStrLn "Bolt"
  exportMeshToObj [] "spiral1.obj" $ removeDoubles [] 0.001 theSpiral1
  putStrLn "Spiral1"
