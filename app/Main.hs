module Main where

import Value
import Point
import Vector
import Mesh
import Models.Bolt
import Models.Spiral

main :: IO()
main = do
  exportMeshToObj [] "bolt.obj" bolt
  putStrLn "Bolt"
  exportMeshToObj [] "spiral1.obj" theSpiral1
  putStrLn "Spiral1"
