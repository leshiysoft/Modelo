module Section where

import Position
import Shape

data Section = Section Position Position

instance Shape Section where
  middle (Section (x1,y1,z1) (x2,y2,z2)) =
    ((x1+x2)/2,(y1+y2)/2,(z1+z2)/2)
