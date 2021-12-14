
import Data.Maybe
import Data.List
import qualified Data.Map as M

type Polymer = String
type Rules = M.Map String Char
type Elements = M.Map Char Integer

main :: IO()
main = interact solve

solve :: String -> String
solve =  show . diff . M.toList . pairInsertions 40 . rules . lines
--solve =  show . pairInsertions 4 . rules . lines

rules :: [String] -> (Polymer, Rules, Elements)
rules xs = (p, M.fromList rs, M.fromList (counts 1 (sort p)))
    where rs = map (\x -> let ws = words x in
                              (ws!!0, head (ws!!2)) ) (drop 2 xs)
          p = head xs

pairInsertions :: Int -> (Polymer, Rules, Elements) -> Elements
pairInsertions n (x, rs, es) = snd $ foldr (\_ (y,z) -> pairInsertion y z rs) (x,es) [1..n]

pairInsertion :: Polymer -> Elements -> Rules -> (Polymer, Elements)
pairInsertion (x:[]) es _ = ([x], es)
pairInsertion (x:y:xs) es rs = (x:el:p, es'')
    where el = fromJust (M.lookup (x:y:[])  rs)
          es' = M.insertWith (+) el 1 es
          (p, es'') = pairInsertion (y:xs) es' rs
counts :: Integer -> Polymer -> [(Char, Integer)]
counts n (x:[]) = [(x, n)]
counts n (x:y:xs) = if x == y then counts (n+1) (y:xs) else (x, n):counts 1 (y:xs)

diff :: [(Char, Integer)] -> Integer
diff xs = snd (last xs') - snd (head xs')
    where xs' = sortOn snd xs

