module Main where

import Value
import Point
import Vector
import Bezier
import Object
import Patch
import Arc
import Sphere
import Models.Bolt

main :: IO()
main = do
  theObject <- return $ mergeObjects $ map (patchToObject (4,4)) bolt
  boltStr <- return $ makeObj theObject
  writeFile "bolt.obj" boltStr
  putStrLn "Bolt"
