
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

type Cuboid = ((Int, Int), (Int, Int), (Int, Int))
type Step = (Bool, Cuboid)

main :: IO()
main = interact solve

solve :: String -> String
solve = show . sum . map onCount . reboot . steps . lines
--solve = show . steps . lines

steps :: [String] -> [Step]
steps = map (step)
    where step xs = (s!!0 == "on", ((read (s!!2), read (s!!3)), (read (s!!5), read (s!!6)), (read (s!!8), read (s!!9))))
            where s = words (map (\y -> if y == '.' || y == ','  || y == '=' then ' ' else y) xs)

reboot :: [Step] -> [Cuboid]
reboot ss = foldr executeSegment [on] ss'
    where (_, c) = head ss'
          ss' = reverse ss
          on = foldr executeExpand c ss'


executeExpand :: Step -> Cuboid -> Cuboid
executeExpand (s, (x,y,z)) c@(x',y',z') = if s then (expand x x', expand y y', expand z z') else c

expand :: (Int, Int) -> (Int, Int) -> (Int, Int)
expand (x, y) (x', y') = (minimum [x,x'], maximum [y,y'])

onCount :: Cuboid -> Int
onCount ((x1, x2), (y1, y2), (z1, z2)) = abs (x2 - x1) * abs (y2 - y1) * abs (z2 - z1)

executeSegment :: Step -> [Cuboid] -> [Cuboid]
executeExpand (s, c) cs = if s then cs else segment c cs

segment :: Cuboid -> [Cuboid] -> [Cuboid]
segment c cs =


