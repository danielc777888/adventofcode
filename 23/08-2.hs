-- TODO
-- store items and l,r in array
-- use array to access elems, O(1) instead of O(log n)

import Data.Array
import Data.List
import qualified Data.Map as M
import Data.Maybe

data BinTree a = Leaf a | Fork a (BinTree a) (BinTree a) deriving (Eq,Show)

type Node = (String, Char)
type Paths = (Int, Int)
type Network = (String, Array Int (Char, Paths))

main :: IO ()
main = interact solve

solve :: String -> String
--solve = show . run 'A' 'Z' . network . B.lines
solve = show . run 'A' 'Z' . network . lines

network :: [String] -> Network
network xs = (head xs, ar)
  where xs' = map node (drop 2 xs)
        m = M.fromList (zip (map fst xs') [0..])
        ar = array (0, length xs' - 1) (zipWith (\i (e, (l,r)) -> (i, (last e, (findNodeIdx m l, findNodeIdx m r)))) [0..] xs')
        node s = case (words (filter (\x -> x /= '=' && x /= ',' && x /= '(' && x /= ')') s)) of
                   [e,l,r] -> (e, (l, r))
                   e       -> error ("invalid node pattern: " ++ show e)

findNodeIdx :: M.Map String Int -> String -> Int
findNodeIdx m s = fromJust (M.lookup s m)

run :: Char -> Char -> Network -> Int
run s e (xs, ar) = run' ar is e (cycle xs) 0
  where is = map fst $ filter ( (s==) . fst . snd) $ assocs ar


run' :: Array Int (Char, Paths) -> [Int] -> Char -> [Char] -> Int -> Int
run' arr ids e (x:xs) n
  | n == 100000000 || b = n
  | otherwise = run' arr ids' e xs (n+1)
  where nodes = map (arr!) ids
        --ids' = map (\(_, ps) -> if x == 'L' then fst ps else snd ps) nodes
        (ids', b) = foldr (\i (ys,b) -> let (c, (l,r)) = arr!i in
                              (if x == 'L' then l:ys else r:ys, if not b || c /= e then False else b)) ([], True) ids

{--
run :: Char -> Char -> Network -> Int
run s e (xs, m) = nodes ts (cycle xs) 0 --run' (ss,s) e m (cycle xs) 0
  where ss = filter (\[_,_,c] -> c == s) (M.keys m)
        ts = map (build m M.empty) ss

build :: M.Map String (String, String) -> M.Map String (BinTree Node) -> String -> BinTree Node
build m cs s
  -- | (last s) == 'Z' = Leaf (s, 'Z')
  | (l,r) == (s,s) = Leaf (s, last s)
  | otherwise = if M.member s cs then fromJust (M.lookup s cs) else f
  where (l,r) = fromJust (M.lookup s m)
        f = Fork (s, last s) lt rt
        lt = build m (M.insert s f cs) l
        rt = build m (M.insert s f cs) r

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
--}
