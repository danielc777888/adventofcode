-- 1061

import Aoc.GameLife
import qualified Data.Map as M
import Data.Maybe

main :: IO ()
main = interact $ solve

solve :: String -> String
solve = show . M.size . M.filter alive . cells . life evolve 100 . board . lines

evolve :: Evolve
evolve b xs (_, x) = case x of
  Alive -> if n == 2 || n == 3 then Alive else Dead
  Dead  -> if n == 3 then Alive else Dead
  Empty -> Empty
  where
    n = length $ filter alive xs

evolve' :: Evolve
evolve' (Board w h _) xs ((x, y), c) = case c of
  Alive -> if alwaysAlive || n == 2 || n == 3 then Alive else Dead
  Dead  -> if n == 3 then Alive else Dead
  Empty -> Empty
  where
    n = length $ filter alive xs
    alwaysAlive = any (== (x, y)) (corners w h)

updateCorners :: Board -> Board
updateCorners b@(Board w h xs) = updatePositions b (corners w h) Alive
