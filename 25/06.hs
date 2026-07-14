import Data.Char
import Data.List

data Prob = Prob [Integer] Op
  deriving Show

data Op = Add | Mult
  deriving Show

main :: IO ()
main = do
  putStrLn "--- Day 6: Trash Compactor ---"
  c <- readFile "06.in"
  let i = input c
  putStrLn $ "Part1: " <> show (part1 i)
  let i2 = input2 c
  putStrLn $ "Part2: " <> show (part2 i2)

input :: String -> [Prob]
input s = map mkProb xss
  where xss = transpose (map words (lines s))

mkProb :: [String] -> Prob
mkProb xs = Prob nums op
  where op = if last xs == "*" then Mult else Add
        nums = map read (init xs)

input2 :: String -> [Prob]
input2 s = mkProbs xss
  where xss = transpose (lines s)

mkProbs :: [String] -> [Prob]
mkProbs [] = []
mkProbs (x:xs)
  | all isSpace x = mkProbs xs
  | otherwise = mkProb2 (x:xs) : mkProbs (dropWhile (not . all isSpace) xs)

mkProb2 :: [String] -> Prob
mkProb2 xs = Prob nums op
  where xs' = takeWhile (not . all isSpace) xs
        nums = map (read . filter (not . isSpace) . init) xs'
        op = if '*' `elem` (safeHead xs') then Mult else Add

safeHead :: [String] -> String
safeHead []    = ""
safeHead (x:_) = x

solve :: Prob -> Integer
solve (Prob nums Add)  = foldr1 (+) nums
solve (Prob nums Mult) = foldr1 (*) nums

-- 6378679666679
part1 :: [Prob] -> Integer
part1 = sum . map solve

-- 11494432585168
part2 :: [Prob] -> Integer
part2 = part1
