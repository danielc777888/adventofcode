import Data.Maybe
import qualified Data.Map as M

type Grid = M.Map Position Char
type Position = (Int, Int)

main :: IO()
main = interact solve

solve :: String -> String
solve = show . snd . simulate 1 . mkGrid . lines

mkGrid :: [String] -> Grid
mkGrid xs = fst $ foldr (\s (m, y) -> (M.union m (mkRow s y), y + 1) ) (M.empty, 0) (reverse xs)
    where mkRow s y = fst $ foldr (\c (m, x) -> (M.insert (x, y) c m, x + 1) ) (M.empty, 0) (reverse s)

simulate :: Int -> Grid -> (Grid, Int)
simulate n m = if (nm + nm') == 0 then (m, n) else simulate (n+1) m''
    where mx = fst $ M.findMax m
          (m', nm) = moveHerd '>' m mx
          (m'', nm') = moveHerd 'v' m' mx

moveHerd :: Char -> Grid -> (Int, Int) -> (Grid, Int)
moveHerd h m mx = M.foldrWithKey (\p c (m', n) -> if c == h then move c p m m' n mx else (m', n) ) (m, 0) m

move :: Char -> Position -> Grid -> Grid -> Int -> (Int, Int) -> (Grid, Int)
move c p m m2 n mx = if blocked then (m2, n) else (m', n+1)
    where ap = adjacent c p mx
          adj = fromJust (M.lookup ap m)
          blocked = not $ adj == '.'
          m' = M.insert p '.' (M.insert ap c m2)

adjacent :: Char -> Position -> (Int, Int) -> Position
adjacent '>' (x, y) (mx, my) = if x == mx then (0, y) else (x+1, y)
adjacent 'v' (x, y) (mx, my) = if y == my then (x, 0) else (x, y+1)

