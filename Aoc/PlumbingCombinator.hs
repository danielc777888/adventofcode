module Aoc.PlumbingCombinator
  ( fork,
  )
where

fork :: (a -> b, a -> c) -> a -> (b, c)
fork (f, g) x = (f x, g x)
