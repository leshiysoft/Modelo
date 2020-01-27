module Section where

import Point
import Vector
import Bezier

data Section = Section {
  sectionStart :: Point,
  sectionEnd :: Point}
  deriving (Eq, Show)

sectionMiddle :: Section -> Point
sectionMiddle s = vectorPoint $ times 0.5 (vStart + vEnd)
  where
    vStart = Vector $ sectionStart s
    vEnd = Vector $ sectionEnd s

sectionToVector :: Section -> Vector
sectionToVector s = vectorBeginEnd (sectionStart s) (sectionEnd s)

sectionBezier :: Point -> Point -> Bezier
sectionBezier a b = toBezier $ Section a b

instance Spline Section where
  toBezier sec@(Section s e) = Bezier (s, v1) (e, v2)
    where
      theVec = sectionToVector sec
      v1 = times (1/3) theVec
      v2 = times (1/3) (-theVec)
