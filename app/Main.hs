module Main where

import Value
import Point
import Vector
import Mesh
import Models.Bolt

main :: IO()
main = do
  exportMeshToObj [] "bolt.obj" $ removeDoubles [] 0.001 bolt
  putStrLn "Bolt"
