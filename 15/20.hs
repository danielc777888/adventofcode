-- mathematics, number theory
import Data.List
import AOC.PlumbingCombinator (fork)

main :: IO()
main = interact $ show . fork (solve1, solve2)

solve1 :: String -> String
solve1 = show . head . houses . read

solve2 :: String -> String
solve2 = id

houses :: Int -> [Int]
houses x =  [h | h <- [1..], presents h >= x]

presents :: Int -> Int
presents = sum . map (*10) . divisors

primes :: [Int]
primes = f [2..] where
  f (p:xs) = p : f [x | x <- xs, not (x `mod` p == 0)]

-- credit goes to https://stackoverflow.com/questions/1480563/making-a-list-of-divisors-in-haskell
divisors :: Int -> [Int]
divisors n = (1:) $ (n:) $ nub $ concat [ [x, n `div` x] | x <- [2..u], n `rem` x == 0 ]
     where u = (floor . sqrt . fromIntegral) n
