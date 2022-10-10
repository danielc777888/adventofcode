import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S

type Grid = M.Map (Int, Int) Int

type Flashes = S.Set (Int, Int)

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . snd . simulate 0 0 . mkGrid . lines

mkGrid :: [String] -> Grid
mkGrid xs = fst $ foldr mkRow (M.empty, nr - 1) xs
  where
    nr = length xs

mkRow :: String -> (Grid, Int) -> (Grid, Int)
mkRow r (m, nr) = (xs, nr - 1)
  where
    nc = length r
    xs = fst $ foldr (\c (n, d) -> (M.insert (d, nr) (digitToInt c) n, d - 1)) (m, nc - 1) r

simulate :: Int -> Int -> Grid -> (Grid, Int)
simulate n nf m = if M.null (M.filter (/= 0) m) then (m, n) else simulate (n + 1) (nf + nf') m'
  where
    (m', nf') = step (M.map (+ 1) m) S.empty

step :: Grid -> Flashes -> (Grid, Int)
step m s
  | S.size s == n' = (m', n')
  | otherwise = step m' s'
  where
    (m', s') = M.foldrWithKey (\k v (x, y) -> if v > 9 && not (S.member k y) then flash k x y else (x, y)) (m, s) m
    n' = S.size s'

flash :: (Int, Int) -> Grid -> Flashes -> (Grid, Flashes)
flash k m s = (m'', s')
  where
    mk = M.findMax m
    ads = adjacent k (fst mk)
    m' = M.insert k 0 m
    s' = S.insert k s
    m'' = foldr (\k acc -> M.adjust (\v -> if v == 0 then v else v + 1) k acc) m' ads

adjacent :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
adjacent (x, y) (nx, ny) = br
  where
    t = if y - 1 < 0 then [] else [(x, y - 1)]
    b = if y + 1 > ny then t else t ++ [(x, y + 1)]
    l = if x - 1 < 0 then b else b ++ [(x - 1, y)]
    r = if x + 1 > nx then l else l ++ [(x + 1, y)]
    tl = if x - 1 < 0 || y - 1 < 0 then r else r ++ [(x - 1, y - 1)]
    tr = if x + 1 > nx || y - 1 < 0 then tl else tl ++ [(x + 1, y - 1)]
    bl = if x - 1 < 0 || y + 1 > ny then tr else tr ++ [(x - 1, y + 1)]
    br = if x + 1 > nx || y + 1 > ny then bl else bl ++ [(x + 1, y + 1)]
