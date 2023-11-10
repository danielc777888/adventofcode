import Data.ByteString.Char8 (pack)
import Data.Char (ord,chr,intToDigit)
import Crypto.Hash (hashWith,MD5 (..))

main :: IO()
main = interact solve

solve :: String -> String
solve = password 0 0

password :: Integer -> Int -> String -> String
password i y xs
  |  y == 8 || i == 10000000 = []
  |  c5 =  c6: password (i+1) (y+1) xs
  |  otherwise = password (i+1) y xs
  where xs' = show $ hashWith MD5 (pack (xs ++ show i))
        c5 = take 5 xs' == "00000"
        c6 = xs' !! 5

toHex :: Char -> String
toHex x = [toHex' y, toHex' r]
  where (y, r) = (ord x) `divMod` 16

toHex' :: Int -> Char
toHex' x
  | x >= 10 && x <= 15 = intToDigit x
  | otherwise = chr (48 + x)
