-- TODO
-- iterat through all elements and instructions
-- save 1st, last element, increment based on length of instructions

import Data.List
import qualified Data.Map as M
import Data.Maybe

type Network = (String, M.Map String (String, String))
type Sims = M.Map String (String, [Int])

main :: IO ()
main = interact solve

solve :: String -> String
--solve = show . run 'A' 'Z' . network . B.lines
solve = show . run . sims . network . lines

sims :: Network -> (String, Sims)
sims (xs, m) = (xs, M.mapWithKey (sim xs' m []) m)
  where xs' = zip [1..] xs

sim :: [(Int, Char)] -> M.Map String (String, String)  -> [Int] -> String -> (String, String) -> (String, [Int])
sim [] _ is ls _ = (ls, sort is)
sim (x:xs) m is ls (l, r) = sim xs m is' ns (fromJust (M.lookup ns m))
  where ns = if snd x == 'L' then l else r
        is' = if last ns == 'Z' then fst x:is else is


network :: [String] -> Network
network xs = (head xs, m)
  where m = M.fromList $ map node $ drop 2 xs
        node s = case (words (filter (\x -> x /= '=' && x /= ',' && x /= '(' && x /= ')') s)) of
          [e,l,r] -> (e, (l, r))
          e       -> error ("invalid node pattern: " ++ show e)

run :: (String, Sims) -> Int
run (xs, m) = run' m ys (length xs) (length ys) 0
  where ys = filter ( ('A'==) . last) $ M.keys m
    --ys = ["AAA"]

run' :: Sims -> [String] -> Int -> Int -> Int -> Int
run'  m xs step lx n
  -- | n > 1000000000 = n
 -- | n > 9003372036854775807 = n
  | length minIdxs > 0 = n + (minimum minIdxs)
  | otherwise = run' m xs' step lx (n + step)
  where ys = map (\x -> fromJust (M.lookup x m)) xs
        xs' = map fst ys
        --ys' = concat $ map snd ys
        --cIdxs = filter ( (==lx) . length) $ groupBy (==) ys'
        minIdxs = foldr (\(_, is) acc -> is `intersect` acc) [1..step] ys

{--
run :: Char -> Char -> Network -> Int
run s e (xs, m) = run' ss e m (cycle xs) 0
  where ss = filter (\x -> last x == s) (M.keys m)

run' :: [String] -> Char -> M.Map String (String, String) -> String -> Int -> Int
run' ss d m (x:xs) n
  | n == 10000000 = n
  | all (\s -> last s == d) ss = n
  | otherwise = run' ss' d m xs (n+1)
    where ss' = map(\s -> let (l,r) = fromJust (M.lookup s m) in
                       if x == 'L' then l else r) ss
run' _ _ _ [] n = n
--}
