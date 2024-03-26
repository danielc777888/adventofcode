--
-- string processing
import Data.List
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)

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
  | n > 100 = n
  | any (\y -> length mol == length y && mol == y) ys = n
  | otherwise = run' (xs, mol) ys' (n+1)
  where ys' = concatMap (\x -> gen x xs) ys

gen :: [Char] -> M.Map String [String] -> [String]
gen s m = concat $ scanl (\acc (pf, (x:xs)) -> let ys = M.lookup [x] m in
                             if isJust ys then map (\y -> pf ++ y ++ xs) (fromJust ys)
                             else [pf ++ (x:xs)]) [] xss
      where xss = init $ zip (inits s) (tails s)
