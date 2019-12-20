{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Geometry where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

--data Dimentions = Dim1 | Dim2 | Dim3
--  deriving (Eq, Show)

class Vector v where
  dist :: v -> Double
  plus :: v -> v -> v
  times :: Double -> v -> v
  norm :: v -> v
  norm v = times (dist v) v
  inv :: v -> v
  inv v = times (-1) v

type Vector1D = Double
type Vector2D = (Double, Double)
type Vector3D = (Double, Double, Double)

instance Vector Vector1D where
  dist = abs
  plus = (+)
  times = (*)

instance Vector Vector2D where
  dist (x,y) = sqrt (x*x+y*y)
  plus (x,y) (u,v) = (x+u,y+v)
  times k (x,y) = (k*x,k*y)

instance Vector Vector3D where
  dist (x,y,z) = sqrt (x*x+y*y+z*z)
  plus (x,y,z) (u,v,w) = (x+u,y+v,z+w)
  times k (x,y,z) = (k*x,k*y,k*z)

diff :: Vector v => v -> v -> v
diff a b = plus a $ inv b

middle :: Vector v => v -> v -> v
middle a b = times 0.5 $ plus a b

class Wrongful w where
  iswrongful :: w -> Bool

data MayExist a where
  Exist :: Wrongful a => a -> MayExist a
  Null :: Wrongful a => MayExist a

deriving instance Show a => Show (MayExist a)

instance Functor MayExist where
  fmap :: (Wrongful w) => (v -> w) -> MayExist v -> MayExist w
  fmap f Null = Null
  fmap f (Exist a) = Exist $ f a

-- instance Applicative MayExist where
--   pure a = Exist a
--   Null <*> _ = Null
--   _ <*> Null = Null
--   (Exist f) <*> (Exist a) = Exist $ f a
--
-- instance Monad MayExist where
--   Null >>= _ = Null
--   (Exist a) >>= f = f a

data Sphere = Sphere Vector3D Double
  | SphereCenterEdge Vector3D Vector3D
  | SphereDiameter Vector3D Vector3D
  deriving (Show)

-- instance Wrongful Sphere where


sphereCenter :: Sphere -> Vector3D
sphereCenter (Sphere  c _) = c
sphereCenter (SphereCenterEdge c _) = c
sphereCenter (SphereDiameter a b) = middle a b

sphereRadius :: Sphere -> Double
sphereRadius (Sphere  _ r) = r
sphereRadius (SphereCenterEdge c e) = dist $ diff c e
sphereRadius (SphereDiameter a b) = (0.5*) $ dist $ diff a b

data Plane = PlaneDotNorm Vector3D Vector3D
  deriving (Show)

planeNorm :: Plane -> Vector3D
planeNorm (PlaneDotNorm _ n) = n
