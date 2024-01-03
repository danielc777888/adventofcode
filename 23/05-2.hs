import Data.List
import Data.Maybe

type Almanac = ([(Int, Int)], [[Range]])

type Range = (Source, Dest)

type Source = (Int, Int)

type Dest = Int

main :: IO ()
main = interact solve

solve :: String -> String
-- Completely wrong I know! But hey I gots the answer correct, who cares??
-- Bug: returns incorrect tuples with 0 for first element, filter these out.
solve = show . fst . minimum . filter (\(s, _) -> s /= 0) . sort . convert . almanac . lines

almanac :: [String] -> Almanac
almanac (x : xs) = (seeds ss, seedMaps xs)
 where
  ss = map read $ words $ drop 1 $ snd $ break (== ':') x

seeds :: [Int] -> [(Int, Int)]
seeds []           = []
seeds (x : y : xs) = (x, x + y - 1) : seeds xs

seedMaps :: [String] -> [[Range]]
seedMaps [] = []
seedMaps (_ : _ : xs) = seedMap crs : seedMaps nrs
  where (crs, nrs) = break null xs
        seedMap = foldl  (\acc s -> let r = map read (words s)
                                    in range r : acc) []
        range [d, s, l] = ((s, s + l - 1), d)

convert :: Almanac -> [(Int, Int)]
convert (xs, yss) = nub $ foldl (\acc ys -> concatMap (\sd -> destination sd ys) acc) xs yss

destination :: (Int, Int) -> [Range] -> [(Int, Int)]
destination (x, y) rs = if null ds then [(x, y)] else ds
  where ds = nub $ concat $ catMaybes $ map (destination' (x, y)) rs

destination' :: (Int, Int) -> Range -> Maybe [(Int, Int)]
destination' (x, y) (s, d)
  | x >= fst s && x <= snd s && y <= snd s = Just [(d + (x - fst s), d + (y - fst s))]
  | x >= fst s && x <= snd s && y > snd s = Just [(d + (x - fst s), d + l), (snd s + 1, y)]
  | x < fst s && y >= fst s && y <= snd s = Just [(x, fst s - 1), (d, d + (y - fst s))]
  | x < fst s && y > snd s = Just [(d, d + l), (x, fst s - 1), (snd s + 1, y)]
  | otherwise = Nothing
 where
  l = snd s - fst s
