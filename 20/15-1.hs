import Data.List
import Data.Maybe

main :: IO ()
main = interact solve

type Turn = (Int, Int)

solve :: String -> String
solve = show . play 2020 . start

start :: String -> [Int]
start [] = []
start xs = [read l :: Int] ++ if null r then [] else start (tail r)
  where
    (l, r) = break (== ',') xs

play :: Int -> [Int] -> Int
play n ns = play' (n - l) zs
  where
    l = length ns
    zs = zip [l, (l - 1) ..] (reverse ns)

play' :: Int -> [Turn] -> Int
play' 0 xs = snd (head xs)
play' n xs = if new then play' (n - 1) ([(l + 1, 0)] ++ xs) else play' (n - 1) ([(l + 1, l - l')] ++ xs)
  where
    (l, r) = head xs
    xs' = tail xs
    mr = mostRecent r xs'
    new = isNothing mr
    (l', r') = fromJust mr

mostRecent :: Int -> [Turn] -> Maybe Turn
mostRecent x xs = find (\(x', y') -> if x == y' then True else False) xs
