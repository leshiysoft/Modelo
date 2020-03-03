module Section where

import Point
import Vector
import Curve
import Shape

data Section = Section {
  sectionStart :: Point,
  sectionEnd :: Point}
  deriving (Show)

sectionMiddle :: Section -> Point
sectionMiddle s = vectorPoint $ times 0.5 (vStart + vEnd)
  where
    vStart = Vector $ sectionStart s
    vEnd = Vector $ sectionEnd s

sectionToVector :: Section -> Vector
sectionToVector s = vectorBeginEnd (sectionStart s) (sectionEnd s)

instance Spline Section where
  toCurve (Section p1 p2) = CurveDir cf cd
    where
      cf v = vectorPoint (times (1-v) (Vector p1) + times v (Vector p2))
      cd v = vectorBeginEnd p1 p2

sectionCurve :: Point -> Point -> Curve
sectionCurve p1 p2 = toCurve $ Section p1 p2

instance Shape Section where
  move v (Section s e) = Section (move v s) (move v e)
