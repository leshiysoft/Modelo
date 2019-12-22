module Shape where

import Position

class Shape s where
  middle :: s -> Position
