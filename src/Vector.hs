module Vector
  (Vector
  ,vector
  ,vectorBeginEnd
  ,dist
  ,plus
  ,times
  ,normalize
  ,inv
  ,cross) where

import Position

data Vector = Vector Position
  deriving Show

vector :: Position -> Vector
vector = Vector

vectorBeginEnd :: Position -> Position -> Vector
vectorBeginEnd p1 p2 = plus (Vector p2) $ inv (Vector p1)

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

inv :: Vector -> Vector
inv v = times (-1) v

cross :: Vector -> Vector -> Vector
cross (Vector (x1,y1,z1)) (Vector (x2,y2,z2)) =
  Vector (z1*y2 - y1*z2, x1*z2 - z1*x2, y1*x2 - x1*y2)
