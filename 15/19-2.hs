--
-- string processing
import Data.List
import qualified Data.Map as M
import Data.Maybe (fromJust)

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . run . parse . lines

parse :: [String] -> (M.Map String [String], String)
parse xs = (M.fromListWith (++) ys', head (drop 1 zs))
  where (ys, zs) = break null xs
        ys' = map (\y -> case (words y) of
                      [k, _, v] -> (k, [v])
                  ) ys

run :: (M.Map String [String], String) -> Int
run  (xs, mol) = run' (xs, mol) es 1
  where es = fromJust (M.lookup "e" xs)

run' :: (M.Map String [String], String) -> [String] -> Int -> Int
run'  (xs, mol) ys n
  | n > 1000000 = n
  | any (\y -> length mol == length y && mol == y) ys = n
  | otherwise = run' (xs, mol) ys' (n+1)
  where ys' = concatMap (\x -> gen x xs) ys

gen :: [Char] -> M.Map String [String] -> [String]
gen s m = concat $ scanl (\acc (pf, (x:xs)) -> let ys = fromJust (M.lookup [x] m) in
                              map (\y -> pf ++ y ++ xs) ys) [] xss
      where xss = init $ zip (inits s) (tails s)
