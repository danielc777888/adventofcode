-- TODO: Took 130s to complete, make faster.
import qualified Data.Map as Map
import Data.Maybe

main :: IO ()
main = interact solve

type Grid4D = Map.Map Coord State

type Grid2D = [Row]

type Row = [HyperCube]

type HyperCube = (Coord, State)

type Coord = (Int, Int, Int, Int)

type State = Bool

cycles :: Int
cycles = 6

solve :: String -> String
solve = show . counts . simulate cycles . grid4D . lines

-- solve = show . counts . grid3D . lines

grid4D :: [String] -> Grid4D
grid4D xs = Map.union i w
  where
    i = Map.fromList (initial xs 0)
    w = Map.fromList (world 16)

write :: Grid4D -> Grid4D -> Grid4D
write g1 g2 = undefined

writeState :: HyperCube -> State -> HyperCube
writeState (c, _) s = (c, s)

initial :: [String] -> Int -> [HyperCube]
initial [] _ = []
initial (y : ys) x = row ++ initial ys (x + 1)
  where
    row = map (\(i, c) -> cube c (x, i, 0, 0)) (zip [0 ..] y)

world :: Int -> [HyperCube]
world d = [((x, y, z, w), False) | z <- [(-d) .. d], x <- [(-d) .. d], y <- [(-d) .. d], w <- [(-d) .. d]]

-- TODO: Must be a more succinct way to do this
neighbours :: Coord -> [Coord]
neighbours (x, y, z, w) = xs ++ ys ++ zs ++ ws ++ xys ++ xzs ++ yzs ++ xws ++ yws ++ zws ++ xyzs ++ xyws ++ xzws ++ yzws ++ xyzws
  where
    xs = [(x + 1, y, z, w), (x - 1, y, z, w)]
    ys = [(x, y + 1, z, w), (x, y - 1, z, w)]
    zs = [(x, y, z + 1, w), (x, y, z - 1, w)]
    ws = [(x, y, z, w + 1), (x, y, z, w - 1)]
    xys = [(x + 1, y + 1, z, w), (x + 1, y - 1, z, w), (x - 1, y - 1, z, w), (x - 1, y + 1, z, w)]
    xzs = [(x + 1, y, z + 1, w), (x + 1, y, z - 1, w), (x - 1, y, z - 1, w), (x - 1, y, z + 1, w)]
    yzs = [(x, y + 1, z + 1, w), (x, y + 1, z - 1, w), (x, y - 1, z - 1, w), (x, y - 1, z + 1, w)]
    xws = [(x + 1, y, z, w + 1), (x + 1, y, z, w - 1), (x - 1, y, z, w - 1), (x - 1, y, z, w + 1)]
    yws = [(x, y + 1, z, w + 1), (x, y + 1, z, w - 1), (x, y - 1, z, w - 1), (x, y - 1, z, w + 1)]
    zws = [(x, y, z + 1, w + 1), (x, y, z + 1, w - 1), (x, y, z - 1, w - 1), (x, y, z - 1, w + 1)]
    xyzs = [(x + 1, y + 1, z + 1, w), (x - 1, y + 1, z + 1, w), (x + 1, y - 1, z + 1, w), (x + 1, y + 1, z - 1, w), (x - 1, y - 1, z + 1, w), (x + 1, y - 1, z - 1, w), (x - 1, y + 1, z - 1, w), (x - 1, y - 1, z - 1, w)]
    xyws = [(x + 1, y + 1, z, w + 1), (x - 1, y + 1, z, w + 1), (x + 1, y - 1, z, w + 1), (x + 1, y + 1, z, w - 1), (x - 1, y - 1, z, w + 1), (x + 1, y - 1, z, w - 1), (x - 1, y + 1, z, w - 1), (x - 1, y - 1, z, w - 1)]
    xzws = [(x + 1, y, z + 1, w + 1), (x - 1, y, z + 1, w + 1), (x + 1, y, z - 1, w + 1), (x + 1, y, z + 1, w - 1), (x - 1, y, z - 1, w + 1), (x + 1, y, z - 1, w - 1), (x - 1, y, z + 1, w - 1), (x - 1, y, z - 1, w - 1)]
    yzws = [(x, y + 1, z + 1, w + 1), (x, y - 1, z + 1, w + 1), (x, y + 1, z - 1, w + 1), (x, y + 1, z + 1, w - 1), (x, y - 1, z - 1, w + 1), (x, y + 1, z - 1, w - 1), (x, y - 1, z + 1, w - 1), (x, y - 1, z - 1, w - 1)]
    xyzws = [(x + 1, y + 1, z + 1, w + 1), (x - 1, y + 1, z + 1, w + 1), (x + 1, y - 1, z + 1, w + 1), (x + 1, y + 1, z - 1, w + 1), (x - 1, y - 1, z + 1, w + 1), (x + 1, y - 1, z - 1, w + 1), (x - 1, y + 1, z - 1, w + 1), (x - 1, y - 1, z - 1, w + 1), (x + 1, y + 1, z + 1, w - 1), (x - 1, y + 1, z + 1, w - 1), (x + 1, y - 1, z + 1, w - 1), (x + 1, y + 1, z - 1, w - 1), (x - 1, y - 1, z + 1, w - 1), (x + 1, y - 1, z - 1, w - 1), (x - 1, y + 1, z - 1, w - 1), (x - 1, y - 1, z - 1, w - 1)]

updateState :: State -> Int -> State
updateState True n = if n == 2 || n == 3 then True else False
updateState False n = if n == 3 then True else False

cube :: Char -> Coord -> HyperCube
cube '.' c = (c, False)
cube '#' c = (c, True)

simulate :: Int -> Grid4D -> Grid4D
simulate 0 g = g
simulate n g =
  simulate
    (n - 1)
    ( Map.mapWithKey
        ( \k x ->
            let ns = neighbours k
                cs = map (\c -> Map.lookup c g) ns
                as = length (filter (\c -> isJust c && fromJust c) cs)
             in updateState x as
        )
        g
    )

counts :: Grid4D -> Int
counts = Map.size . Map.filter (== True)

state :: HyperCube -> Bool
state (_, x) = x

group :: [a] -> Int -> [[a]]
group [] _ = []
group xs n = take n xs : group (drop n xs) n
