import qualified Data.Map as M
import Data.Maybe

type Network = (String, M.Map String (String, String))

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . length . run "AAA" "ZZZ" . network . lines

network :: [String] -> Network
network xs = (head xs, m)
  where m = M.fromList $ map node $ drop 2 xs
        node s = case (words (filter (\x -> x /= '=' && x /= ',' && x /= '(' && x /= ')') s)) of
          [e,l,r] -> (e, (l, r))
          e       -> error ("invalid node pattern: " ++ show e)

run :: String -> String -> Network -> [Char]
run s d (xs, m) = run' s d m (cycle xs)

run' :: String -> String -> M.Map String (String,String) -> [Char] -> [Char]
run' _ _ _ [] = []
run' s d m (x:xs)
  | s == d = []
  | otherwise = x: if x == 'L' then run' l d m xs else run' r d m xs
    where (l,r) = fromJust (M.lookup s m)
