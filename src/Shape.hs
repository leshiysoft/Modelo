{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Shape where

import Vector
import Point
import Value

class Shape s where
  move :: Vector -> s -> s

instance Shape Point where
  move v p = vectorPoint (v + Vector p)

moveForward :: Shape s => Value -> s -> s
moveForward v s = move (times v forward) s

moveBackward :: Shape s => Value -> s -> s
moveBackward v s = move (times v backward) s

moveUp :: Shape s => Value -> s -> s
moveUp v s = move (times v up) s

moveDown :: Shape s => Value -> s -> s
moveDown v s = move (times v down) s

moveRight :: Shape s => Value -> s -> s
moveRight v s = move (times v right) s

moveLeft :: Shape s => Value -> s -> s
moveLeft v s = move (times v left) s
