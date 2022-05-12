--TODO : Not complete, takes too long

import qualified Data.Set as Set

main :: IO()
main = interact solve

type Deck = [Int]

solve :: String -> String
solve = show . score .  winner . play Set.empty . decks . lines
--solve = show . play Set.empty . decks . lines

decks :: [String] -> (Deck, Deck)
decks (x:xs) = (map read l, map read (drop 2 r))
  where (l, r) = break null xs

play :: Set.Set (Deck, Deck) -> (Deck, Deck) -> (Deck, Deck)
play _ ([], xs) = ([], xs)
play _ (xs, []) = (xs, [])
play ds (x:xs, y:ys)
   |  Set.member (x:xs, y:ys) ds = (x:xs, [])
   |  length xs >= x && length ys >= y = if player1Wins p then p1 else p2
   |  x > y = p1 
   |  x < y = p2
   | otherwise = error "dunno what to do here"
   where ds' = Set.insert (x:xs, y:ys) ds
         p = play Set.empty (xs, ys)
         p1 = play ds' (xs ++ [x, y], ys)
         p2 = play ds' (xs, ys ++ [y, x])

winner :: (Deck, Deck) -> Deck
winner ([], xs)  = xs
winner (xs, []) = xs
winner (xs, _) = xs

player1Wins :: (Deck, Deck) -> Bool
player1Wins (xs, []) = True
player1Wins _  = False

score :: Deck -> Int
score xs = foldr (\(x, y) acc -> (x * y) + acc ) 0 (zip xs [l, l-1..1])
  where l = length xs
