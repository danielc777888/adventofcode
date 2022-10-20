-- string processing
-- part 1 correct
-- part 2 threw in towel
{-# LANGUAGE OverloadedStrings #-}

import AOC.PlumbingCombinator (fork)
import Control.Applicative
import Control.Arrow
import Data.ByteString.Char8 qualified as C
import Data.List
import Data.List.Split
import Data.Map qualified as M
import Data.Maybe
import Data.Semigroup
import Data.Set qualified as S

type Replacements = M.Map C.ByteString [C.ByteString]

main :: IO ()
main = interact $ show . fork (solve1, solve3)

solve1 :: String -> String
solve1 = show . S.size . molecules . replacements . C.lines . C.pack

solve2 :: String -> String
solve2 = show . steps . replacements . C.lines . C.pack

replacements :: [C.ByteString] -> (C.ByteString, Replacements)
replacements xs = (mol, reps)
  where
    mol = head $ reverse xs
    reps = M.fromListWith (++) $ map rep (drop 2 $ reverse xs)
    rep x = case (C.words x) of
      [c, _, r] -> (c, [r])

molecules :: (C.ByteString, Replacements) -> S.Set C.ByteString
molecules x = molecules' x (C.empty, fst x) S.empty

molecules' :: (C.ByteString, Replacements) -> (C.ByteString, C.ByteString) -> S.Set C.ByteString -> S.Set C.ByteString
molecules' (x, xs) (a, b) s
  | C.null b = s
  | M.member c1 xs = molecules' (x, xs) (a' 1, b' 1) (s' c1 1)
  | M.member c2 xs = molecules' (x, xs) (a' 2, b' 2) (s' c2 2)
  | otherwise = molecules' (x, xs) (a' 1, b' 1) s
  where
    c1 = C.take 1 b
    c2 = C.take 2 b
    a' n = C.append a (C.take n b)
    b' n = C.drop n b
    s' k n = foldl' (\acc r -> S.insert (C.append a (C.append r (C.drop n b))) acc) s $ fromJust $ M.lookup k xs

steps :: (C.ByteString, Replacements) -> Int
steps (x, xs) = minimum ys
  where
    xs' = S.fromList $ fromJust $ M.lookup "e" xs
    (ys, s) = steps' (x, C.length x, xs) xs' 1 []

steps' :: (C.ByteString, Int, Replacements) -> S.Set C.ByteString -> Int -> [Int] -> ([Int], S.Set C.ByteString)
steps' (x, l, xs) ys n zs
  | n == 250 = (zs, ys)
  | n < l = steps' (x, l, xs) ys' (n + 1) zs
  | S.member x ys = (n : zs, ys)
  | otherwise = (zs, ys)
  where
    ys' =
      S.foldl'
        ( \acc m ->
            let ms = molecules (m, xs)
             in S.union acc ms
        )
        S.empty
        (S.take 100 ys)


