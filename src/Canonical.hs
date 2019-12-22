module Canonical where

class Canonical c where
  canon :: c -> c
