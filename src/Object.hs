module Object where

import Position

data Object = Object
  {objVertexes :: [Position]
  ,objFaces :: [Int]
  }

-- makeObj :: Object -> String
