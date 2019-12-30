module Vector where

import Position

data Vector = Vector {vectorPosition :: Position}
  deriving Show

-- definitions

vector :: Position -> Vector
vector = Vector

vectorBeginEnd :: Position -> Position -> Vector
vectorBeginEnd p1 p2 = plus (Vector p2) $ inv (Vector p1)

-- functions

nonEmptyVector :: Vector -> Maybe Vector
nonEmptyVector (Vector p)
  | p == origin = Nothing
  | otherwise = Just $ Vector p

dist :: Vector -> Double
dist (Vector (x,y,z)) = sqrt (x*x+y*y+z*z)

plus :: Vector -> Vector -> Vector
plus (Vector (x,y,z)) (Vector (u,v,w)) =
  Vector (x+u,y+v,z+w)

times :: Double -> Vector -> Vector
times k (Vector (x,y,z)) = Vector (k*x,k*y,k*z)

normalize :: Vector -> Maybe(Vector)
normalize v
  | len <= 0 = Nothing
  | otherwise = Just $ times (1/len) v
  where len = dist v

normalizeTo :: Double -> Vector -> Maybe(Vector)
normalizeTo d v = times d <$> normalize v

inv :: Vector -> Vector
inv v = times (-1) v

cross :: Vector -> Vector -> Vector
cross (Vector (x1,y1,z1)) (Vector (x2,y2,z2)) =
  Vector (z1*y2 - y1*z2, x1*z2 - z1*x2, y1*x2 - x1*y2)

dot :: Vector -> Vector -> Double
dot (Vector (x1,y1,z1)) (Vector (x2,y2,z2)) = x1*x2+y1*y2+z1*z2

mix :: Vector -> Vector -> Vector -> Double
mix a b c = dot a $ cross b c
