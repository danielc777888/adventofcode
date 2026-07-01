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
--  putStrLn $ "Part2: " <> show (part2 (fst i))

input :: String -> [Prob]
input s = map mkProb xss
  where xss = transpose (map words (lines s))

mkProb :: [String] -> Prob
mkProb xs = Prob nums op
  where op = if last xs == "*" then Mult else Add
        nums = map read (init xs)

solve :: Prob -> Integer
solve (Prob nums Add) = foldr1 (+) nums
solve (Prob nums Mult) = foldr1 (*) nums

-- 
part1 :: [Prob] -> Integer
part1  = sum . map solve

--
--part2 :: [Range] -> Nat
--part2  = sum . map (\(x, y) -> (y - x) + 1) . mergeRanges
