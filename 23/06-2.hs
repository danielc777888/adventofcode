import Data.List

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . product . filter (>0) . map (length . winners . distances) .  races . lines

races :: [String] -> [(Int,Int)]
races (x:y:[]) = [(ts, ds)]
  where ts = read $ intercalate "" $ drop 1 $ words x
        ds = read $ intercalate "" $ drop 1 $ words y

distances :: (Int,Int) -> (Int, [Int])
distances (x,d) = (d,ds)
  where ds = [y * (x-y) | y <- [0..x]]

winners :: (Int, [Int]) -> [Int]
winners (x, xs) = filter (>x) xs
