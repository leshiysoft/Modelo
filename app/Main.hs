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



main2 :: IO ()
main2 = do
  theLines <- return [theArc] -- [theLeft, theRight, theStart, theEnd]
  carcas <- return $ map (\b -> bezierToObject 100 b) theLines
  str1 <- return $ makeObj $ mergeObjects carcas
  writeFile "f1.obj" str1
  str2 <- return $ makeObj $ patchToObject (10,10) thePatch
  writeFile "f2.obj" str2
  putStrLn "I'm Modelo"


theLeft = Bezier ((-1,0,0), Vector (-1,0,1)) ((-1,0,5), Vector (-1,0,-1))
theRight = Bezier ((1,0,0), Vector (-1,0,1)) ((1,0,5), Vector (-1,0,-1))
theStart = Bezier ((-1,0,0), Vector (0,1,0)) ((1,0,0), Vector (0,1,0))
theEnd = Bezier ((-1,0,5), Vector (0,1,0)) ((1,0,5), Vector (0,1,0))

thePatch = Patch theLeft theRight (ts1,ts2) (te1,te2)
  where
    Bezier (_,ts1) (_,ts2) = theStart
    Bezier (_,te1) (_,te2) = theEnd

theMiddle = patchBezier thePatch 0.5


middles :: [Bezier]
middles = map (patchBezier thePatch) $ map (\v -> fromIntegral v / 100) [1..99]
  where
    pt = Patch theLeft theRight
      ( Vector (0,1,0), Vector (0,1,0)) ( Vector (0,3,0), Vector (0,3,0))


theArc :: Bezier
theArc = arcToBezier $ Arc (Sphere (0,0,0) 10) (Vector (0,0,1)) (Vector (1,0,0))
