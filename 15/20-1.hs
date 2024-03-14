-- 776160
-- mathematics, number theory

import Data.Array
import Data.List

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . head . houses . read

houses :: Int -> [Int]
houses x = [h | h <- [1 ..], presents h >= x]

presents :: Int -> Int
presents = sum . map (* 10) . divisors

elves :: Int -> Int
elves x = elves' x 1 200000 arr
  where
    arr = array (1, 800000) [(x, 0) | x <- [1 .. 800000]]

elves' :: Int -> Int -> Int -> Array Int Int -> Int
elves' _ _ 0 arr = maximum (elems arr) -- 0
elves' x e m arr = if h > 0 then h else elves' x (e + 1) (m - 1) arr'
  where
    hs = filter (<= 800000) $ map (* e) [1 .. 50]
    p = e * 11
    (h, arr') = elf hs x p [] arr

elf :: [Int] -> Int -> Int -> [(Int, Int)] -> Array Int Int -> (Int, Array Int Int)
elf [] _ _ ps arr = (0, arr // ps)
elf (h : hs) x p ps arr = if p + np >= x then (h, arr) else elf hs x p ps' arr
  where
    np = arr ! h
    ps' = (h, p + np) : ps

primes :: [Int]
primes = f [2 ..]
  where
    f (p : xs) = p : f [x | x <- xs, not (x `mod` p == 0)]

divisors :: Int -> [Int]
divisors n = (1 :) $ (n :) $ nub $ concat [[x, n `div` x] | x <- [2 .. u], n `rem` x == 0]
  where
    u = (floor . sqrt . fromIntegral) n
