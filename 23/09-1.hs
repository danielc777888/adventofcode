
import Data.List (unfoldr)

type History = [Int]

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . sum . map (extrapolate . diffs . map read . words) . lines

diffs :: History -> [History]
diffs xs = unfoldr step xs
           where step ys | null ys = Nothing
                         | all (==0) ys = Just (ys, [])
                         | otherwise = Just (ys, diff ys)

diff :: History -> History
diff xs = zipWith (\x y -> y - x) xs (tail xs)

extrapolate :: [History] -> Int
extrapolate  = foldr (+) 0 . map last
