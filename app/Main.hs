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
  exportMeshtoObj "bolt.obj" bolt
  putStrLn "Bolt"
