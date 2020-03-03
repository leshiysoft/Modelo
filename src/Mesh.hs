module Mesh where

import Point
import Value
import Data.List
import Vector

data Mesh = Mesh
  {meshVertexes :: [Point]
  ,meshFaces :: [[Int]]
  ,meshLines :: [(Int, Int)]
  }
  deriving Show

exportMeshToObj :: ParameterList -> String -> Mesh -> IO ()
exportMeshToObj plist path mesh  = result
  where
    result = exportMeshToObj' plist  path $ removeDoubles plist 0.001 mesh

exportMeshToObj' :: ParameterList -> String -> Mesh -> IO ()
exportMeshToObj' plist path (Mesh vxs fs ls) = do writeFile path content
  where
    vtos v = show ((/100000) $ fromIntegral $ round (extract plist v * 100000))
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

removeDoubles :: ParameterList -> Double -> Mesh -> Mesh
removeDoubles _ _ m@(Mesh [] _ _) = m
removeDoubles _ _ m@(Mesh (_:[]) _ _) = m
removeDoubles plist delta (Mesh vtx fs ls) = result
  where
    eq p1 p2 = delta >= extract plist (dist $ vectorBeginEnd p1 p2)
    lv = last vtx
    result = case any (eq lv) (init vtx) of
      False -> Mesh (vtx2 ++ [lv]) fs2 ls2
        where
          Mesh vtx2 fs2 ls2 = removeDoubles plist delta (Mesh (init vtx) fs ls)
      True -> removeDoubles plist delta (Mesh (init vtx) fs2 ls2)
        where
          ((_,ind):_) = filter (eq lv . fst) $ zip vtx [1..]
          remap1 x = if (x == length vtx) then ind else x
          remap2 x = if (x > length vtx) then x-1 else x
          fs2 = map (map (remap2 . remap1)) fs
          ls2 = map (\(x,y) -> (remap2 $ remap1 x, remap2 $ remap1 y)) ls
