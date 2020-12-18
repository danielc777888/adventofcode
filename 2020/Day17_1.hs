import qualified Data.Map as Map
import Data.Maybe

main :: IO()
main = interact solve

--type Grid3D = [Grid2D]
type Grid3D = Map.Map Coord State
type Grid2D = [Row]
type Row = [Cube]
type Cube  = (Coord, State)
type Coord = (Int, Int, Int)
type State = Bool

cycles :: Int
cycles = 6

solve :: String -> String
solve = show . counts . simulate cycles . grid3D . lines
--solve = show . counts . grid3D . lines

grid3D :: [String] -> Grid3D
grid3D xs = Map.union i w
  where i = Map.fromList (initial xs 0)
        w = Map.fromList (world 30)

write :: Grid3D -> Grid3D -> Grid3D
write g1 g2 = undefined

writeState :: Cube -> State -> Cube
writeState (c, _) s = (c, s)

initial :: [String] -> Int -> [Cube]
initial [] _ = []
initial (y:ys) x = row ++ initial ys (x+1)
  where row = map(\(i, c) -> cube c (x, i, 0)) (zip [0..] y)

world :: Int -> [Cube]
--world x y = group (group xs gs) gs
world d = [((x, y, z), False) | z <- [(-d)..d], x <- [(-d)..d], y <- [(-d)..d]]
  --where d = 30--x * y
        --gs = (d * 2) + 1

neighbours :: Coord -> [Coord]
neighbours (x, y, z) = xs ++ ys ++ zs ++ xys ++ xzs ++ yzs ++ xyzs
  where xs = [(x + 1, y, z), (x - 1, y, z)]
        ys = [(x, y + 1, z), (x, y - 1, z)]
        zs = [(x, y, z + 1), (x, y, z - 1)]
        xys = [(x + 1, y + 1, z), (x + 1, y - 1, z), (x - 1, y - 1, z), (x - 1, y + 1, z)]
        xzs = [(x + 1, y, z + 1), (x + 1, y, z - 1), (x - 1, y, z - 1), (x - 1, y, z + 1)]
        yzs = [(x, y + 1, z + 1), (x, y + 1, z - 1), (x, y - 1, z - 1), (x, y - 1, z + 1)]
        xyzs = [(x + 1, y + 1, z + 1), (x - 1, y + 1, z + 1), (x + 1, y  - 1, z + 1), (x + 1, y + 1, z - 1), (x - 1, y - 1, z + 1), (x + 1, y - 1, z - 1), (x - 1, y + 1, z - 1), (x - 1, y - 1, z -1)]
        

updateState :: State -> Int -> State
updateState True n = if n == 2 || n == 3 then True else False
updateState False n = if n == 3 then True else  False

cube :: Char -> Coord -> Cube
cube '.' c =  (c, False)
cube '#' c =  (c, True)

simulate :: Int -> Grid3D  -> Grid3D
simulate 0 g = g
simulate n g = simulate (n-1) (Map.mapWithKey(\k x -> let ns = neighbours k
                                                          cs = map (\c -> Map.lookup c g) ns
                                                          as = length (filter (\c -> isJust c && fromJust c) cs)
                                                       in updateState x as)  g)

counts :: Grid3D -> Int
counts = Map.size . Map.filter (==True)

state :: Cube -> Bool
state (_, x) = x

group :: [a] -> Int -> [[a]]
group [] _ = []
group xs n = take n xs:group (drop n xs) n
