--
-- string processing
import Data.List
import qualified Data.Map as M
import Data.Maybe (fromJust)

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . parse . lines

parse :: [String] -> (M.Map String [String], String)
parse xs = (M.fromListWith (++) ys', head (drop 1 zs))
  where (ys, zs) = break null xs
        ys' = map (\y -> case (words y) of
                      [k, _, v] -> (k, [v])
                  ) ys

{--
run :: Int -> String -> (Map String [String], String) -> Int
run n x (xs, mol) = if x == mol then n else replace xs mol ys 1
  where ys = fromJust (lookup x xs)

replace :: Int -> String -> (Map String [String], String) -> Int
replace n mol (xs, mol')
  | n > 10000000 = n
  | mol == mol' = n
--}

gen :: [Char] -> M.Map String [String] -> [String]
gen s m = concat $ scanl (\acc (pf, (x:xs)) -> let ys = fromJust (M.lookup [x] m) in
                              map (\y -> pf ++ y ++ xs) ys) [] xss
      where xss = init $ zip (inits s) (tails s)
