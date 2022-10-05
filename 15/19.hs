-- string processing
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

import Data.List
import Data.Maybe
import Control.DeepSeq
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C
import AOC.PlumbingCombinator (fork)

-- type Replacement = (C.ByteString, C.ByteString)
type Replacements = M.Map C.ByteString [C.ByteString]

main :: IO ()
main = interact $ show . fork (solve1, solve2)

solve1 :: String -> String
solve1 = show . S.size . molecules . replacements . C.lines . C.pack

solve2 :: String -> String
solve2 =  show  . steps . replacements . C.lines . C.pack

replacements :: [C.ByteString] -> (C.ByteString, Replacements)
replacements xs = (mol, reps)
  where mol = head $ reverse xs
        reps = M.fromListWith (++) $ map rep (drop 2 $ reverse xs)
        rep x = case (C.words x) of
          [c, _, r] -> (c, [r])

{-
molecules :: (C.ByteString, Replacements) -> S.Set C.ByteString
molecules (x, xs) = foldl' (replace ys) S.empty xs
  where ys = zip (C.inits x) (C.tails x)
        replace :: [(C.ByteString, C.ByteString)] -> S.Set C.ByteString -> Replacement -> S.Set C.ByteString
        replace mols s (r, p) =  foldl' (\acc (a, b) -> let p1 = if C.null b then C.empty else C.singleton $ C.head b
                                                            p2 = if C.null b then C.empty else C.take 2 b in
                                                          if r == p2 || r ==  p1 then S.insert (mconcat [a, p, C.drop (C.length r) b]) acc else acc) s mols
-}


molecules :: (C.ByteString, Replacements) -> S.Set C.ByteString
molecules x = molecules' x (C.empty, fst x) S.empty

molecules' :: (C.ByteString, Replacements) -> (C.ByteString, C.ByteString) -> S.Set C.ByteString -> S.Set C.ByteString
molecules' (x, xs) (a, b) s
  | C.null b = s
  | M.member c1 xs = molecules' (x, xs) (a' 1, b' 1) (s' c1 1)
  | M.member c2 xs = molecules' (x, xs) (a' 2, b' 2) (s' c2 2)
  | otherwise = molecules' (x, xs) (a' 1, b' 1) s
  where c1 = force $ C.take 1 b
        c2 = force $ C.take 2 b
        a' n = force $ C.append a (C.take n b)
        b' n = force $ C.drop n b
        s' k n = force $ foldl' (\acc r -> S.insert (C.append a (C.append r (C.drop n b))) acc ) s $ fromJust $ M.lookup k xs
        
steps :: (C.ByteString, Replacements) -> Int
steps (x, xs) = minimum $ steps' (x, C.length x, xs) xs' 1 []
  where xs' = S.fromList $ fromJust $ M.lookup "e" xs

steps' :: (C.ByteString, Int, Replacements) -> S.Set C.ByteString -> Int -> [Int] -> [Int]
steps' (x, l, xs) ys n zs
 | n `seq` n > l = zs
 | n `seq` n < l =  steps' (x, l, xs) (force ys') (n+1) zs
 | S.member x ys = n:zs
 | otherwise = zs
 where ys' = S.foldl' (\acc m -> let ms = molecules (m, xs) in
                           S.union acc (force ms)) S.empty ys
 


