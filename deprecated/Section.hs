module Section where

import Position
import Shape

data Section = Section {
  sectionBegin :: Position,
  sectionEnd :: Position}

-- definitions

section :: Position -> Position -> Section
section = Section

-- instances

instance Shape Section where
  middle (Section (x1,y1,z1) (x2,y2,z2)) =
    ((x1+x2)/2,(y1+y2)/2,(z1+z2)/2)

-- functions

nonEmptySection :: Section -> Maybe Section
nonEmptySection s@(Section p1 p2)
  | p1 == p2 = Nothing
  | otherwise = Just s
