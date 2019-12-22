module Vector where

import Position
import Canonical

data Vector = Vector Position
  | Vector2Dots Position Position
  deriving Show

instance Canonical Vector where
  canon v@(Vector _) = v
  canon (Vector2Dots p1 p2) =
    plus (Vector p2) $ inv (Vector p1)

dist :: Vector -> Double
dist (Vector (x,y,z)) = sqrt (x*x+y*y+z*z)
dist v = dist $ canon v

plus :: Vector -> Vector -> Vector
plus (Vector (x,y,z)) (Vector (u,v,w)) =
  Vector (x+u,y+v,z+w)
plus v1 v2 = plus (canon v1) (canon v2)

times :: Double -> Vector -> Vector
times k (Vector (x,y,z)) = Vector (k*x,k*y,k*z)
times k v = times k $ canon v

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
