module Wrongful where

class Wrongful w where
  check :: w -> Maybe w
