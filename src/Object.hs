module Object where

import Position
import Data.List

data Object = Object
  {objVertexes :: [Position]
  ,objFaces :: [[Int]]
  }
  deriving Show

makeObj :: Object -> String
makeObj (Object vxs fs) = vpart ++ fpart
  where
    vpart = concat $ map (\(x,y,z) -> intercalate " " ["v", show x, show y, show z, "\n"] ) vxs
    fpart = concat $ map (\f -> "f " ++ intercalate " " (map show f) ++ "\n") fs

mergeObjects :: [Object] -> Object
mergeObjects [] = Object [] []
mergeObjects (h:t) = Object (vtx ++ vtx2) (fs ++ fs2')
  where
    Object vtx fs = h
    Object vtx2 fs2 = mergeObjects t
    fs2' = map (map (+(length vtx))) fs2
