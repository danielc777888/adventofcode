import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import AOC.PlumbingCombinator (fork)

type Replacement = (String, String)
type Replacements = [Replacement]

main :: IO()
main = interact $ show . fork (solve1, solve2)

solve1 :: String -> String
solve1 = show . S.size . molecules . replacements . lines

solve2 :: String -> String
solve2 =  show  . steps . replacements . lines

replacements :: [String] -> (String, Replacements)
replacements xs = (mol, reps)
  where mol = head $ reverse xs
        reps = map rep (drop 2 $ reverse xs)
        rep x = case (words x) of
          [c, _, r] -> (c, r)

molecules :: (String, Replacements) -> S.Set String
molecules (x, xs) = foldr (replace ys) S.empty xs
  where ys = zip (inits x) (tails x)
        replace :: [(String, String)] -> Replacement -> S.Set String -> S.Set String
        replace mols (r, p) s =  foldr (\(a, b) acc -> if r `isPrefixOf` b then S.insert (concat (a:p:[drop (length r) b])) acc else acc) s mols

steps :: (String, Replacements) -> Int
steps (x, xs) = steps' (x, xs) xs' 1
  where !xs' = S.fromList $ map snd $ filter ( (== "e") . fst) xs

steps' :: (String, Replacements) -> S.Set String -> Int -> Int
steps' (x, xs) ys n
 | n > 100 = -1
 | S.member x ys = n
 | otherwise = steps' (x, xs) ys' (n+1)
 where !ys' = S.foldr' (\m acc -> S.union acc (molecules (m, xs) )) S.empty ys



