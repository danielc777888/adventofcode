-- TODO
-- use normal array instead of map
-- use  mutable array
-- use unboxed mutal array
-- use word instead of int for indexes
-- use vector


import Data.Array
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Vector as V

type Network = (String, M.Map String (String, String))
type Sims = Array Node (String, Node, String, [Node])
type Node = Int

main :: IO ()
main = interact solve

solve :: String -> String
--solve = show . run 'A' 'Z' . network . B.lines
solve = show . run . sims . network . lines

sims :: Network -> (String, Sims)
sims (xs, m) = (xs, fromMap  (M.mapWithKey (sim xs' m []) m))
  where xs' = zip [1..] xs

sim :: [(Node, Char)] -> M.Map String (String, String)  -> [Node] -> String -> (String, String) -> (String, [Node])
sim [] _ is ls _ = (ls, sort is)
sim (x:xs) m is _ (l, r) = sim xs m is' ns (fromJust (M.lookup ns m))
  where ns = if snd x == 'L' then l else r
        --is' = if last ns == 'Z' || (last ls == 'Z' && fst x == 1) then fst x:is else is
        is' = if last ns == 'Z' then fst x:is else is

fromMap :: M.Map String (String, [Node]) -> Array Node (String, Node, String, [Node])
fromMap m = array (1,fromIntegral (length xs)) xs'
  where xs = zip [1..] (map (\(s, (e, is)) -> (s,0,e,is)) (M.toList m))
        xs' = map (\(i,(s,_,e,is)) -> let ei = findIdx e xs in
                        (i,(s,ei,e,is))) xs


findIdx :: String -> [(Node, (String,Node,String,[Node]))] -> Node
findIdx s xs = fst $ fromJust $ find (\(_,(s',_,_,_)) -> s' == s) xs

startName :: (String, Node, String, [Node]) -> String
startName (x, _, _, _) = x

network :: [String] -> Network
network xs = (head xs, m)
  where m = M.fromList $ map node $ drop 2 xs
        node s = case (words (filter (\x -> x /= '=' && x /= ',' && x /= '(' && x /= ')') s)) of
          [e,l,r] -> (e, (l, r))
          e       -> error ("invalid node pattern: " ++ show e)


run :: (String, Array Node (String,Node,String, [Node])) -> Int
run (xs, a) = run' v ys (length xs) (length ys) 0
  where ys = map fst $ filter (\(_,e) -> (last (startName e)) =='A') $ assocs a
        a' = array (bounds a) $ map (\(i, (_,n,_,ns)) -> (i, (n,ns))) $ assocs a
        v = V.fromList (elems a')
        --ys = map fst $ filter (\(i,e) -> startName e == "AAA") $ assocs a
        --ys = ["AAA"]

run' :: V.Vector (Node,[Node]) -> [Node] -> Int -> Int -> Int -> Int
run' v xs step lx n
     | n > 100000000000 = n
  -- | n > 14289612900000 = n
     | isJust minIdx = n + (fromJust minIdx)
     | otherwise = run' v xs' step lx (n + step)
  where xs' = map (\x -> fst (V.unsafeIndex v (x-1))) xs
        ys' = sort $ concatMap (\x -> snd (V.unsafeIndex v (x-1))) xs
        minIdx = findMin ys' lx 1

findMin :: [Node] -> Int -> Int -> Maybe Node
findMin [] _ _ = Nothing
findMin (x:[]) t c = if t == c then Just x else Nothing
findMin (x:y:xs) t c
  | x == y && (c+1) == t = Just x
  | x == y = findMin (y:xs) t (c+1)
  | otherwise = findMin (y:xs) t 1
