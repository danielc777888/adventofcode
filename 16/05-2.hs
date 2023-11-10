import Data.ByteString.Char8 (pack)
import Data.Char (ord,chr,intToDigit, digitToInt)
import Data.List (splitAt)

import Crypto.Hash (hashWith,MD5 (..))

main :: IO()
main = interact solve

solve :: String -> String
solve = password 0 0 "zzzzzzzz"

password :: Integer -> Int -> String -> String -> String
password i y pw xs
  |  i == 30000000 || not ('z' `elem` pw) = pw
  |  ih =  password (i+1) (y+1) pw' xs
  |  otherwise = password (i+1) y pw xs
  where xs' = show $ hashWith MD5 (pack (xs ++ show i))
        ih = take 5 xs' == "00000"
        pos = digitToInt $ xs' !! 5
        c7 = xs' !! 6
        pw' = if pos > 7 || (pw !! pos /= 'z') then pw else (s1 ++ [c7] ++ drop 1 s2)
        (s1, s2) = splitAt pos pw

toHex :: Char -> String
toHex x = [toHex' y, toHex' r]
  where (y, r) = (ord x) `divMod` 16

toHex' :: Int -> Char
toHex' x
  | x >= 10 && x <= 15 = intToDigit x
  | otherwise = chr (48 + x)
