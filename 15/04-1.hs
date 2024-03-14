-- 254575

import Crypto.Hash
import Data.ByteString.Char8 (pack)
import Data.Char
import Data.List
import System.Environment (getArgs)

type Key = (Int, String)

type Hash = (Key, String)

type Bit = Int

main :: IO ()
main = interact solve

solve :: String -> String
solve _ = show (lowestNumber (hashKeys (genKeys "bgvyzdsv")))

genKeys :: String -> [Key]
genKeys xs = zip [1 .. 10000000] (repeat xs)

hashKeys :: [Key] -> [Hash]
hashKeys = filter (\x -> leadingZeroes (snd x) 5) . map (\x -> (x, show (md5 (pack (showKey x)))))
  where
    md5 x = hash x :: Digest MD5
    key (x, y) = x ++ show y

showKey :: Key -> String
showKey (x, y) = y ++ (show x)

leadingZeroes :: String -> Int -> Bool
leadingZeroes xs n = if take n xs == take n (repeat '0') && head (drop n xs) /= '0' then True else False

lowestNumber :: [Hash] -> Int
lowestNumber = head . sort . map (fst . fst)
