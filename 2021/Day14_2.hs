
import Data.Maybe
import Data.List
import qualified Data.Map as M

type Polymer = String
type Rules = M.Map String Char

main :: IO()
main = interact solve

solve :: String -> String
-- solve =  show . diff . counts 1 . sort . pairInsertion 40 . rules . lines
solve =  show . pairInsertion 1 . rules . lines

rules :: [String] -> (Polymer, Rules)
rules xs = (head xs, M.fromList rs)
    where rs = map (\x -> let ws = words x in
                              (ws!!0, head (ws!!2)) ) (drop 2 xs)

    {-
pairInsertions :: Int -> (Polymer, Rules) -> (Polymer, Rules)
pairInsertions 0 (p, rs) = (p, rs)
pairInsertions n (p, rs) = pairInsertions (n-1) (p', rs)
    where xs = chunk p
          h = head p
          p' = h:concatMap (\x -> drop 1(pairInsertion x rs)) xs
          -}

pairInsertion :: Int -> (Polymer,Rules) -> Polymer
-- pairInsertion n (x:[]) _ = [x]
pairInsertion n (x:[], _) = [x]
pairInsertion 0 (p, _) = p
pairInsertion n ((x:y:xs), rs) = pairInsertion (n-1) ([x,el], rs) ++ pairInsertion (n-1) ((y:xs), rs)
    where el = fromJust (M.lookup (x:y:[])  rs)

counts :: Integer -> Polymer -> [(Integer, Char)]
counts n (x:[]) = [(n, x)]
counts n (x:y:xs) = if x == y then counts (n+1) (y:xs) else (n, x):counts 1 (y:xs)

diff :: [(Integer, Char)] -> Integer
diff xs = fst (last xs') - fst (head xs')
    where xs' = sort xs

chunk :: Polymer -> [Polymer]
chunk [] = [[]]
chunk (x:[]) = [[]]
chunk (x:y:[]) = [[x,y]]
chunk (x:y:xs) = [[x,y]] ++ chunk (y:xs)
