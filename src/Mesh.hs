module Mesh where

import Point
import Value
import Data.List

data Mesh = Mesh
  {meshVertexes :: [Point]
  ,meshFaces :: [[Int]]
  ,meshLines :: [(Int, Int)]
  }
  deriving Show

exportMeshtoObj :: String -> Mesh -> IO ()
exportMeshtoObj path (Mesh vxs fs ls) = do writeFile path content
  where
    vtos v = show ((/100000) $ fromIntegral $ round (extract [] v * 100000))
    vpart = concat $ map (\(x,y,z) -> intercalate " " ["v", vtos x, vtos y, vtos z, "\n"] ) vxs
    fpart = concat $ map (\f -> "f " ++ intercalate " " (map show f) ++ "\n") fs
    lpart =concat $ map (\(l1,l2) -> "l " ++ show l1 ++ " " ++ show l2 ++ "\n") ls
    content = vpart ++ fpart ++ lpart

mergeMeshes :: [Mesh] -> Mesh
mergeMeshes [] = Mesh [] [] []
mergeMeshes (h:t) = Mesh (vtx ++ vtx2) (fs ++ fs2') (ls ++ ls2')
  where
    Mesh vtx fs ls = h
    Mesh vtx2 fs2 ls2 = mergeMeshes t
    adding = length vtx
    fs2' = map (map (+adding)) fs2
    ls2' = map (\(l1,l2) -> (l1+adding,l2+adding)) ls2
