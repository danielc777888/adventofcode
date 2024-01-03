import Data.List

type Almanac = ([Int], [[Range]])

type Range = (Source, Dest)

type Source = (Int, Int)

type Dest = Int

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . minimum . convert . almanac . lines

almanac :: [String] -> Almanac
almanac (x : xs) = (seeds x, seedMaps xs)
  where seeds = map read . words . drop 1 . snd . break (== ':')

seedMaps :: [String] -> [[Range]]
seedMaps [] = []
seedMaps (_ : _ : xs) = seedMap crs : seedMaps nrs
  where (crs, nrs) = break null xs
        seedMap = foldl (\acc s -> let r = map read (words s)
                                   in range r : acc) []
        range [d, s, l] = ((s, s + l - 1), d)

convert :: Almanac -> [Int]
convert (xs, yss) = foldl (\acc ys -> map (\sd -> destination sd ys) acc) xs yss

destination :: Int -> [Range] -> Int
destination x rs = maybe x (\(s, d) -> d + (x - fst s)) mr
  where mr = find (\(s, _) -> x >= fst s && x <= snd s) rs
