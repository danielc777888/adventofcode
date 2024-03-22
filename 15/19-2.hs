--
-- string processing
import Data.Map (Map, fromListWith)

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . parse . lines

parse :: [String] -> (Map String [String], String)
parse xs = (fromListWith (++) ys', head (drop 1 zs))
  where (ys, zs) = break null xs
        ys' = map (\y -> case (words y) of
                      [k, _, v] -> (k, [v])
                  ) ys


run :: (Map String [String], String) -> Int
run = undefined
