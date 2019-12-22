module Plane where

import Position
import Vector
import Canonical
import Wrongful

data Plane = Plane Position Vector
  | PlanePosVecVec Position Vector Vector
  deriving (Show)

instance Canonical Plane where
  canon p@(Plane _ _) = p
  canon (PlanePosVecVec p v1 v2) =
    Plane p $ cross v1 v2

instance Wrongful Plane where
  check p
    | (dist $ planeNormal p) <= 0 = Nothing
    | otherwise = Just p

planeNormal :: Plane -> Vector
planeNormal p = let (Plane _ n) = canon p in n
