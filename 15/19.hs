import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import AOC.PlumbingCombinator (fork)

data Answer = forall a. Show a => Answer a

instance Show Answer where
  show (Answer s) = show s 

type Replacement = String
type Replacements = M.Map String [Replacement]

main :: IO()
main = interact $ show . fork (solve1, id)

solve1 :: String -> Answer
solve1 = Answer . length . nub . replace . replacements . lines

solve2 :: String -> Answer
solve2 = Answer . minimum . filter (>= 0) . steps . replacements . lines

replacements :: [String] -> (String, Replacements)
replacements xs = (mol, reps)
  where mol = head $ reverse xs
        reps = foldr(\x acc -> let (k, v) = rep x in
                        M.insertWith (++) k [v] acc) M.empty (drop 2 $ reverse xs)
        rep x = case (words x) of
          [c, _, r] -> (c, r)

replace :: (String, Replacements) -> [String]
replace (x, rs) = concat $ replace' (x, rs) ([], x) [[]]

replace' :: (String, Replacements) -> (String, String) -> [[String]] -> [[String]]
replace' (o, rs) (b, a) xss =
  case a of
    (x:y:ys) -> replace' (o, rs) ( (b ++ [x]), (y:ys)) (g1:g2:xss)
    (x:[]) -> (g1:xss)
    [] -> xss
  where g1 = gens [head a] rs (b, (drop 1 a))
        g2 = gens (take 2 a) rs (b, (drop 2 a))

gens :: String -> Replacements -> (String,String) -> [String]
gens x rs (b, a) = if M.member x rs then map (\v -> b ++ v ++ a) vs else []
  where vs = fromJust $ M.lookup x rs

steps :: (String, Replacements) -> [Int]
steps (x, xs) = steps' (x, xs) es 1
  where es = S.fromList $ fromJust $ M.lookup "e" xs

steps' :: (String, Replacements) -> S.Set String -> Int -> [Int]
steps' (x, xs) ys n
  | S.member x ys = [n]
  | n > 100 = [-1]
  | otherwise = steps' (x, xs) ys' (n + 1)
    where ys' = S.fromList $ concat $ S.map (\y -> replace (y, xs)) ys




