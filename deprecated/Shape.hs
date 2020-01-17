module Shape where

import Position
import Vector

class Shape s where
  middle :: s -> Position
