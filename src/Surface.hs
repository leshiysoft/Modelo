module Surface where

import Value
import Point
import Mesh
import Shape

data Surface = Surface (Value -> Value -> Point)

--TODO: релизовать внутреннее значение разбиения

surfaceFunction :: Surface -> (Value -> Value -> Point)
surfaceFunction (Surface sf) = sf

--TODO: реализовать конвертацию в Mesh
-- surfaceToMesh :: Int -> Int -> Surface -> Mesh
-- surfaceToMesh i j

class Surfaceable s where
  toSurface :: s -> Surface

instance Shape Surface where
  move v (Surface sf) = Surface (\v1 v2 -> move v $ sf v1 v2)
