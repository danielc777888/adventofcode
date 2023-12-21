import Data.List

type Almanac = ([(Int,Int)],[[Range]])
type Range = (Range, Dest)
type Source = (Int,Int)
type Dest = Int

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . minimum . convert . almanac . lines

almanac :: [String] -> Almanac
almanac (x:xs) = (concat (seeds ss), seedMaps xs)
  where ss = map read $ words $ drop 1 $ snd $ break (==':') x
--        seeds = [s1..s1+l1-1] ++ [s2..s2+l2-1]
seeds :: [Int] -> [[(Int,Int)]]
seeds []       = [[]]
seeds (x:y:xs) = (x,x+y-1):seeds xs

seedMaps :: [String] -> [[Range]]
seedMaps [] = []
seedMaps (_:_:xs) = seedMap crs: seedMaps nrs
  where (crs,nrs) = break null xs
        seedMap = foldl (\acc s -> let r = map read (words s) in
                            range r: acc) []
        range [d,s,l] = ((s, s+l-1), d)


convert :: Almanac -> [Int]
convert (xs, yss) = foldl (\acc ys -> concatMap (\sd -> destination sd ys) acc) xs yss

destination :: (Int,Int) -> [Range] -> [Int]
destination (x,y) rs = maybe x (\(s,d) -> d + (x - fst s)) mr
  where mr = find (\(s,_) -> x >= fst s && x <= snd s) rs
