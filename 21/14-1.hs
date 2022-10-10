import Data.List
import qualified Data.Map as M
import Data.Maybe

type Polymer = String

type Rules = M.Map String Char

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . diff . counts 1 . sort . pairInsertions 10 . rules . lines

-- solve =  show . pairInsertions 4 . rules . lines

rules :: [String] -> (Polymer, Rules)
rules xs = (head xs, M.fromList rs)
  where
    rs =
      map
        ( \x ->
            let ws = words x
             in (ws !! 0, head (ws !! 2))
        )
        (drop 2 xs)

pairInsertions :: Int -> (Polymer, Rules) -> Polymer
pairInsertions n (x, rs) = foldr (\_ y -> pairInsertion y rs) x [1 .. n]

pairInsertion :: Polymer -> Rules -> Polymer
pairInsertion (x : []) _ = [x]
pairInsertion (x : y : xs) rs = x : el : pairInsertion (y : xs) rs
  where
    el = fromJust (M.lookup (x : y : []) rs)

counts :: Int -> Polymer -> [(Int, Char)]
counts n (x : []) = [(n, x)]
counts n (x : y : xs) = if x == y then counts (n + 1) (y : xs) else (n, x) : counts 1 (y : xs)

diff :: [(Int, Char)] -> Int
diff xs = fst (last xs') - fst (head xs')
  where
    xs' = sort xs
