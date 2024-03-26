-- 260

import qualified Data.Set as S

main :: IO()
main = interact solve

solve :: String -> String
solve = show . length . filter snd . map(\x -> (x, ssl x)) . lines

ssl :: String ->  Bool
ssl xs = go 0 xs S.empty S.empty
  where go ib [] _ _  = False
        go ib (x:xs) s1 s2
          | x == '[' = go (ib+1) xs s1 s2
          | x == ']' = go (ib-1) xs s1 s2
          | isPattern xs' && ib == 0 = if S.member (corr xs') s2 then True else go ib xs (S.insert xs' s1) s2
          | isPattern xs' && ib > 0  = if S.member (corr xs') s1 then True else go ib xs s1 (S.insert xs' s2)
          | otherwise = go ib xs s1 s2
         where xs' = take 3 (x:xs)
               cs = corr xs'

isPattern :: String -> Bool
isPattern [a,b,c] = a == c && a /= b
isPattern _       = False

corr :: String -> String
corr [a,b,c] = [b, a, b]
corr xs      = xs
