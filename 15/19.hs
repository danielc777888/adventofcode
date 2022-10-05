-- string processing
{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.Maybe
import Control.DeepSeq
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import AOC.PlumbingCombinator (fork)

type Replacement = (T.Text, T.Text)
type Replacements = [Replacement]

main :: IO ()
main = interact $ show . fork (solve1, id)

solve1 :: String -> String
solve1 = show . S.size . molecules . replacements . T.lines . T.pack

solve2 :: String -> String
solve2 =  show  . steps . replacements . T.lines . T.pack

replacements :: [T.Text] -> (T.Text, Replacements)
replacements xs = (mol, reps)
  where mol = head $ reverse xs
        reps = map rep (drop 2 $ reverse xs)
        rep x = case (T.words x) of
          [c, _, r] -> (c, r)

molecules :: (T.Text, Replacements) -> S.Set T.Text
molecules (x, xs) = foldl' (replace ys) S.empty xs
  where ys = zip (T.inits x) (T.tails x)
        replace :: [(T.Text, T.Text)] -> S.Set T.Text -> Replacement -> S.Set T.Text
        replace mols s (r, p) =  foldl' (\acc (a, b) -> let p1 = if T.null b then T.empty else T.singleton $ T.head b
                                                            p2 = if T.null b then T.empty else T.take 2 b in
                                                          if r == p2 || r ==  p1 then S.insert (mconcat [a, p, T.drop (T.length r) b]) acc else acc) s mols

steps :: (T.Text, Replacements) -> Int
steps (x, xs) = minimum $ steps' (x, T.length x, xs) xs' 1 []
  where xs' = S.fromList $ map snd $ filter ( (== "e") . fst) xs

steps' :: (T.Text, Int, Replacements) -> S.Set T.Text -> Int -> [Int] -> [Int]
steps' (x, l, xs) ys n zs
 | n `seq` n > l = zs
 | n `seq` n < l = ys' `deepseq` steps' (x, l, xs) ys' (n+1) zs
 | S.member x ys = n:zs
 | otherwise = zs
 where !ys' = S.foldl' (\acc m -> S.union acc (molecules (m, xs) )) S.empty ys
 


