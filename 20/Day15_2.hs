import Data.Maybe
import Data.List
import qualified Data.Map as Map

main :: IO()
main = interact solve

type Turn = (Int, Int)

solve :: String -> String
solve = show . play 30000000 . start

start :: String -> [Int]
start [] = []
start xs = [read l::Int] ++ if null r then [] else start (tail r)
  where (l, r) = break (==',') xs

play :: Int -> [Int] -> Int
play n ns = play' (n - l) zs mp
  where l = length ns
        zs = zip [l,(l-1)..] (reverse ns)
        mp = Map.fromList (map(\(x, y) -> (y, x)) zs)

play' :: Int -> [Turn] -> Map.Map Int Int -> Int
play' 0 xs _ = snd (head xs)
play' n xs mp = if new then play' (n-1) ((l+1, 0):xs) mp' else play' (n-1) ((l+1, l-l'):xs) mp'
  where (l, r) = head xs
        mr = Map.lookup r mp
        new = isNothing mr
        l' = fromJust mr
        mp' = Map.insert r l mp
