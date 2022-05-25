
import Data.List
import qualified Data.Map as M

type Cuboid = M.Map (Int, Int, Int) Bool
type Step = (Bool, (Int, Int), (Int, Int), (Int, Int))

main :: IO()
main = interact solve

solve :: String -> String
solve = show . length . M.filter id . reboot (-50, 50) . steps . lines
--solve = show . steps . lines

steps :: [String] -> [Step]
steps = map (step)
    where step xs = (s!!0 == "on", (read (s!!2), read (s!!3)), (read (s!!5), read (s!!6)), (read (s!!8), read (s!!9)))
            where s = words (map (\y -> if y == '.' || y == ','  || y == '=' then ' ' else y) xs)

reboot :: (Int, Int) -> [Step] -> Cuboid
reboot bs ss = foldr (execute bs) M.empty (reverse ss)

execute :: (Int, Int) -> Step -> Cuboid -> Cuboid
execute (x, y) (s, (x1, x2), (y1, y2), (z1, z2)) m = M.union (M.fromList [((x, y, z), s) | x <- clampx, y <- clampy, z <- clampz]) m
    where clampx = [x..y] `intersect` [x1..x2]
          clampy = [x..y] `intersect` [y1..y2]
          clampz = [x..y] `intersect` [z1..z2]
