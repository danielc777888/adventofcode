import qualified Data.Map as M
import Data.Char

type HeightMap = M.Map (Int, Int) Int

main :: IO()
main = interact solve

solve :: String -> String
solve = show . sum . riskLevels . lowPoints . heightMap M.empty 0 . lines

heightMap :: HeightMap -> Int -> [String] -> HeightMap
heightMap m r [] = m
heightMap m r (x:xs) = heightMap m' (r+1) xs
    where hs = map digitToInt x
          ks = zip (repeat r) [0..(length hs)-1]
          m' =  foldr(\(k,v) n -> M.insert k v n) m (zip ks hs)

lowPoints :: HeightMap -> [Int]
lowPoints m = map snd (M.toList m')
    where m' = M.filterWithKey(\k v -> lowPoint k v m) m

lowPoint :: (Int, Int) -> Int -> HeightMap -> Bool
lowPoint (r, c) v m = all (v<) vs
                where vs = adjacent (r,c) m

adjacent :: (Int, Int) -> HeightMap -> [Int]
adjacent (r, c) m = [up, down, left, right]
    where up = findHeight (r-1, c) m
          down = findHeight (r+1, c) m
          left = findHeight (r, c-1) m
          right = findHeight (r, c+1) m
          findHeight k n = M.findWithDefault 10 k n

riskLevels :: [Int] -> [Int]
riskLevels = map (+1)
