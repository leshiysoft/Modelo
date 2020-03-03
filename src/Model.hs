module Model where

import Mesh

type DiscretizerList = [(String, Int)]

--TODO: наследовать от Shape
data Model = Model (DiscretizerList -> Mesh)
