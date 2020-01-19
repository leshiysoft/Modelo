module Object where

import Point
import Value
import Data.List

data Object = Object
  {objVertexes :: [Point]
  ,objFaces :: [[Int]]
  ,objLines :: [(Int, Int)]
  }
  deriving Show

makeObj :: Object -> String
makeObj (Object vxs fs ls) = vpart ++ fpart ++ lpart
  where
    vpart = concat $ map (\(x,y,z) -> intercalate " " ["v", vtos x, vtos y, vtos z, "\n"] ) vxs
    fpart = concat $ map (\f -> "f " ++ intercalate " " (map show f) ++ "\n") fs
    lpart =concat $ map (\(l1,l2) -> "l " ++ show l1 ++ " " ++ show l2 ++ "\n") ls

vtos :: Value -> String
vtos v = show ((/100000) $ fromIntegral $ round (extract [] v * 100000))

mergeObjects :: [Object] -> Object
mergeObjects [] = Object [] [] []
mergeObjects (h:t) = Object (vtx ++ vtx2) (fs ++ fs2') (ls ++ ls2')
  where
    Object vtx fs ls = h
    Object vtx2 fs2 ls2 = mergeObjects t
    adding = length vtx
    fs2' = map (map (+adding)) fs2
    ls2' = map (\(l1,l2) -> (l1+adding,l2+adding)) ls2
