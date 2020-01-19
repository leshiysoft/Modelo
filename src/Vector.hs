module Vector where

import Point
import Value

data Vector = Vector {vectorPoint :: Point}
  deriving (Eq, Show)

instance Num Vector where
  (+) (Vector (x1,y1,z1)) (Vector (x2,y2,z2)) = Vector (x1+x2,y1+y2,z1+z2)
  (*) (Vector (x1,y1,z1)) (Vector (x2,y2,z2)) = Vector (x1*x2,y1*y2,z1*z2)
  abs (Vector (x,y,z)) = Vector (abs x, abs y, abs z)
  signum (Vector (x,y,z)) = Vector (signum x, signum y, signum z)
  fromInteger i = let a = fromInteger i in Vector (a,a,a)
  negate (Vector (x,y,z)) = Vector (negate x, negate y, negate z)

vectorBeginEnd :: Point -> Point -> Vector
vectorBeginEnd p1 p2 = Vector p2 - Vector p1

move :: Vector -> Point -> Point
move v p = vectorPoint (v + Vector p)

dist :: Vector -> Value
dist (Vector (x,y,z)) = sqrt (x*x+y*y+z*z)

times :: Value -> Vector -> Vector
times k v = (Vector (k,k,k)) * v

normalize :: Vector -> Vector
normalize vec = times (recip $ dist vec) vec

normalizeTo :: Value -> Vector -> Vector
normalizeTo v vec = times (v / dist vec) vec

cross :: Vector -> Vector -> Vector
cross (Vector (x1,y1,z1)) (Vector (x2,y2,z2)) =
  Vector (z1*y2 - y1*z2, x1*z2 - z1*x2, y1*x2 - x1*y2)

dot :: Vector -> Vector -> Value
dot (Vector (x1,y1,z1)) (Vector (x2,y2,z2)) = x1*x2+y1*y2+z1*z2

mix :: Vector -> Vector -> Vector -> Value
mix a b c = dot a $ cross b c

nullVector :: Vector
nullVector = Vector origin
