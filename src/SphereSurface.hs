module SphereSurface where

-- deprecated module!!!!!!!!!!!!!!!!!!!!!!!!

import Sphere
import Vector
import Object

data SphereSurface = SphereSurfaceTri Sphere Vector Vector Vector
  | SphereSurfaceQuad Sphere Vector Vector Vector Vector
  deriving Show

-- definitions

sphereSurfaceTri :: Sphere -> Vector -> Vector -> Vector -> Maybe SphereSurface
sphereSurfaceTri s a b c
  | (sphereRadius s <= 0) || (mix a b c <= 0) = Nothing
  | otherwise = Just $ SphereSurfaceTri s a b c

sphereSurfaceQuad :: Sphere -> Vector -> Vector -> Vector -> Vector -> Maybe SphereSurface
sphereSurfaceQuad s a b c d
  | (sphereRadius s <= 0) || ((mix a b c <= 0) && (mix b c d <= 0)) = Nothing
  | otherwise = Just $ SphereSurfaceQuad s a b c d

quad :: SphereSurface -> [SphereSurface]
quad surf@(SphereSurfaceQuad _ _ _ _ _) = [surf]
quad surf@(SphereSurfaceTri s a b c) = [q1, q2, q3]
  where
    abc = plus a $ plus b c
    ab = plus a b
    ac = plus a c
    bc = plus b c
    q1 = SphereSurfaceQuad s a ab abc ac
    q2 = SphereSurfaceQuad s b bc abc ab
    q3 = SphereSurfaceQuad s c ac abc bc

toObject :: SphereSurface -> Object
toObject (SphereSurfaceQuad s a b c d) = Object vxs [[1,2,3,4]]
  where
    (Just vxs) = sequence $ map (spherePoint s) [a,b,c,d]
