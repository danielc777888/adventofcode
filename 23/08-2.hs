-- TODO
-- get counts of when condition met
--

import Data.List
import qualified Data.Map as M
import Data.Maybe


data BinTree a = Leaf a | Fork a (BinTree a) (BinTree a) deriving Show

type Node = (String, Char)

type Network = (String, M.Map String (String, String))

main :: IO ()
main = interact solve

solve :: String -> String
--solve = show . run 'A' 'Z' . network . B.lines
solve = show . run 'A' 'Z' . network . lines

network :: [String] -> Network
network xs = (head xs, m)
  where m = M.fromList $ map node $ drop 2 xs
        node s = case (words (filter (\x -> x /= '=' && x /= ',' && x /= '(' && x /= ')') s)) of
          [e,l,r] -> (e, (l, r))
          e       -> error ("invalid node pattern: " ++ show e)


run :: Char -> Char -> Network -> Int
run s e (xs, m) = nodes ts (cycle xs) 0 --run' (ss,s) e m (cycle xs) 0
  where ss = filter (\[_,_,c] -> c == s) (M.keys m)
        ts = map (build m 100000000) ss

build :: M.Map String (String, String) -> Int -> String -> BinTree Node
build m n s
  -- | (last s) == 'Z' = Leaf (s, 'Z')
  | (l,r) == (s,s) || n == 0 = Leaf (s, last s)
  | otherwise = Fork (s, last s) (build m (n-1) l) (build m (n-1) r)
  where (l,r) = fromJust (M.lookup s m)


nodes :: [BinTree Node] -> String -> Int -> Int
nodes ts (x:xs) !n
  | n == 100000000 = n
  | all target ts = n
  | otherwise = nodes (map (nav x) ts) xs (n+1)

target :: BinTree Node -> Bool
target (Leaf (_,'Z'))     = True
target (Fork (_,'Z') _ _) = True
target _                  = False

nav :: Char -> BinTree Node -> BinTree Node
nav _ (Leaf x)     = error ("Cannot nav from a Leaf node : " ++ show x)
nav c (Fork _ l r) = if c == 'L' then l else r

run' :: ([String], Char) -> Char -> M.Map String (String, String) -> String -> Int -> Int
run' (ss,c) d m (x:xs) n
  | n == 10000000 = n
  | c == d = n
  | otherwise = run' ss' d m xs (n+1)
    where ss' = foldl' (\(ys, d') s ->
                         let (l,r) = fromJust (M.lookup s m)
                             s'@[_,_,c] = if x == 'L' then l else r
                         in (s':ys, if d' == 'z' || c == d' then d' else 'z')) ([],d) ss
--    where ss' = map(\s -> let (l,r) = fromJust (M.lookup s m) in
  --                     if x == 'L' then l else r) ss
run' _ _ _ [] n = n



{--

          Fork("11A",'A')
      (Fork ("11B",'B')
(Leaf ("XXX",'X'))(Leaf ("11Z",'Z'))) (Leaf ("XXX",'X'))



Fork ("22A",'A') (Fork ("22B",'B') (Fork ("22C",'C') (Leaf ("22Z",'Z')) (Leaf ("22Z",'Z'))) (Fork ("22C",'C') (Leaf ("22Z",'Z')) (Leaf ("22Z",'Z')))) (Leaf ("XXX",'X'))
--}
