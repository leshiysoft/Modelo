module Combinations where

byPairs :: (a -> a -> b) -> [a] -> [b]
byPairs f (a:b:[]) = [f a b]
byPairs f (a:b:t) = f a b : byPairs f (b:t)
