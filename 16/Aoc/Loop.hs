module Aoc.Loop
  ( apply,
  )
where

apply :: Int -> (a -> a) -> a -> a
apply 0 f = id
apply n f = apply (n - 1) f . f
