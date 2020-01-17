module Extra where

import Section
import Vector

vectorSection :: Section -> Vector
vectorSection (Section p1 p2) = vectorBeginEnd p1 p2
