import Data.Char
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

type HeightMap = M.Map (Int, Int) Int

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . product . take 3 . reverse . sort . basins . lowPoints . heightMap M.empty 0 . lines

heightMap :: HeightMap -> Int -> [String] -> HeightMap
heightMap m r [] = m
heightMap m r (x : xs) = heightMap m' (r + 1) xs
  where
    hs = map digitToInt x
    ks = zip (repeat r) [0 .. (length hs) - 1]
    m' = foldr (\(k, v) n -> M.insert k v n) m (zip ks hs)

lowPoints :: HeightMap -> (HeightMap, HeightMap)
lowPoints m = (m', m)
  where
    m' = M.filterWithKey (\k v -> lowPoint k v m) m

lowPoint :: (Int, Int) -> Int -> HeightMap -> Bool
lowPoint (r, c) v m = all (v <) vs
  where
    vs = map snd $ adjacent (r, c) m

adjacent :: (Int, Int) -> HeightMap -> [((Int, Int), Int)]
adjacent (r, c) m = [up, down, left, right]
  where
    up = ((r - 1, c), findHeight (r - 1, c) m)
    down = ((r + 1, c), findHeight (r + 1, c) m)
    left = ((r, c - 1), findHeight (r, c - 1) m)
    right = ((r, c + 1), findHeight (r, c + 1) m)
    findHeight k n = M.findWithDefault 10 k n

basins :: (HeightMap, HeightMap) -> [Int]
basins (lps, m) = map (fst . expand m S.empty) (M.toList lps)

expand :: HeightMap -> S.Set (Int, Int) -> ((Int, Int), Int) -> (Int, S.Set (Int, Int))
expand m s ((r, c), v)
  | v == 9 || v == 10 || S.member (r, c) s = (0, s)
  | otherwise = (1 + n, s'')
  where
    ads = adjacent (r, c) m
    s' = S.insert (r, c) s
    (n, s'') =
      foldr
        ( \((x, y), z) (a, b) ->
            let (a', b') = expand m b ((x, y), z)
             in (a + a', b')
        )
        (0, s')
        ads
