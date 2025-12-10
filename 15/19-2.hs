-- 535
-- string processing
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as C
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S

type Replacements = M.Map C.ByteString [C.ByteString]

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . S.size . molecules . replacements . C.lines . C.pack

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
