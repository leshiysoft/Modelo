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

det :: Point -> Point -> Point -> Value
det (a11, a12,a13) (a21, a22,a23) (a31, a32,a33) =
  a11*a22*a33 - a11*a23*a32 - a12*a21*a33 + a12*a23*a31 + a13*a21*a32 - a13*a22*a31

toBasis :: (Vector, Vector, Vector) -> Vector -> Vector
toBasis (x, y, z) p = result
  where 
    (x1,x2,x3) = vectorPoint x
    (y1,y2,y3) = vectorPoint y
    (z1,z2,z3) = vectorPoint z
    (p1,p2,p3) = vectorPoint p
    d = det (x1,y1,z1) (x2,y2,z2) (x3,y3,z3)
    d1 = det (p1,y1,z1) (p2,y2,z2) (p3,y3,z3)
    d2 = det (x1,p1,z1) (x2,p2,z2) (x3,p3,z3)
    d3 = det (x1,y1,p1) (x2,y2,p2) (x3,y3,p3)
    result = Vector (d1/d,d2/d,d3/d)
