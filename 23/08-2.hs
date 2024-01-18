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
import Data.Word

type Network = (String, M.Map String (String, String))
--type Sims = M.Map String (String, [Int])
type Sims = Array Node (String, Node, String, [Node])
type Node = Word16

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
sim (x:xs) m is ls (l, r) = sim xs m is' ns (fromJust (M.lookup ns m))
  where ns = if snd x == 'L' then l else r
        is' = if last ns == 'Z' || (last ls == 'Z' && fst x == 1) then fst x:is else is

fromMap :: M.Map String (String, [Node]) -> Array Node (String, Node, String, [Node])
fromMap m = array (1,fromIntegral (length xs)) xs'
  where xs = zip [1..] (map (\(s, (e, is)) -> (s,0,e,is)) (M.toList m))
        xs' = map (\(i,(s,_,e,is)) -> let ei = findIdx e xs in
                        (i,(s,ei,e,is))) xs


findIdx :: String -> [(Node, (String,Node,String,[Node]))] -> Node
findIdx s xs = fst $ fromJust $ find (\(_,(s',_,_,_)) -> s' == s) xs

startName :: (String, Node, String, [Node]) -> String
startName (x, _, _, _) = x

endIdx :: (String, Node, String, [Node]) -> Node
endIdx (_, x, _, _) = x

endName :: (String, Node, String, [Node]) -> String
endName (_, _, x, _) = x

idxs :: (String, Node, String, [Node]) -> [Node]
idxs (_, _, _, x) = x


network :: [String] -> Network
network xs = (head xs, m)
  where m = M.fromList $ map node $ drop 2 xs
        node s = case (words (filter (\x -> x /= '=' && x /= ',' && x /= '(' && x /= ')') s)) of
          [e,l,r] -> (e, (l, r))
          e       -> error ("invalid node pattern: " ++ show e)


run2 :: (String, Array Node (String,Node,String, [Node])) -> [(Node, (String,Node,String,[Node]))]
run2 (xs, a) =  filter (\(i,e) -> length (idxs e) > 0) $ assocs a

run :: (String, Array Node (String,Node,String, [Node])) -> Int
run (xs, a) = run' a ys (length xs) (length ys) 0
  where ys = map fst $ filter (\(i,e) -> (last (startName e)) =='A') $ assocs a
       -- ys = map fst $ filter (\(i,e) -> startName e == "AAA") $ assocs a
    --  ys = ["AAA"]

run' :: Array Node (String,Node,String,[Node]) -> [Node] -> Int -> Int -> Int -> Int
run' a xs step lx n
  -- | n > 1000000000000 = n
     | n > 14289612809129 = n
 -- | n > 9003372036854775807 = n
     | length cIdxs > 0 = n + fromIntegral (head (head cIdxs))
     | otherwise = run' a xs' step lx (n + step)
  where ys = map (\x -> a!x) xs
        xs' = map endIdx ys
        ys' = concat $ map idxs ys
        cIdxs = filter ( (==lx) . length) $ groupBy (==) ys'
