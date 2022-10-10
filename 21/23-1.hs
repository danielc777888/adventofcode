import Data.Array

type Burrow = Array Int (Maybe Amphipod)

data Amphipod
  = Amber
  | Bronze
  | Copper
  | Desert
  deriving (Show)

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . minimum . moves . burrow . lines

burrow :: [String] -> Burrow
burrow = undefined

moves :: Burrow -> [Int]
moves = undefined

organized :: Burrow -> Bool
organized = undefined

energy :: Amphipod -> Int
energy Amber = 1
energy Bronze = 10
energy Copper = 100
energy Desert = 1000
