import AOC.GameOfLife
import AOC.PlumbingCombinator (fork)
import Data.Map qualified as M
import Data.Maybe

main :: IO ()
main = interact $ show . fork (solve1, solve2)

solve1 :: String -> String
solve1 = show . M.size . M.filter alive . cells . life evolve 100 . board . lines

solve2 :: String -> String
solve2 = show . M.size . M.filter alive . cells . life evolve' 100 . updateCorners . board . lines

evolve :: Evolve
evolve b xs (_, x) = case x of
  Alive -> if n == 2 || n == 3 then Alive else Dead
  Dead -> if n == 3 then Alive else Dead
  Empty -> Empty
  where
    n = length $ filter alive xs

evolve' :: Evolve
evolve' (Board w h _) xs ((x, y), c) = case c of
  Alive -> if alwaysAlive || n == 2 || n == 3 then Alive else Dead
  Dead -> if n == 3 then Alive else Dead
  Empty -> Empty
  where
    n = length $ filter alive xs
    alwaysAlive = any (== (x, y)) (corners w h)

updateCorners :: Board -> Board
updateCorners b@(Board w h xs) = updatePositions b (corners w h) Alive
