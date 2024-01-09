import qualified Data.Map as M
import Data.Maybe

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
