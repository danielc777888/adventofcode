main :: IO ()
main = interact solve

type Deck = [Int]

solve :: String -> String
solve = show . score . play . decks . lines

decks :: [String] -> (Deck, Deck)
decks (x : xs) = (map read l, map read (drop 2 r))
  where
    (l, r) = break null xs

play :: (Deck, Deck) -> Deck
play ([], xs) = xs
play (xs, []) = xs
play (x : xs, y : ys) = if x > y then play (xs ++ [x, y], ys) else play (xs, ys ++ [y, x])

score :: Deck -> Int
score xs = foldr (\(x, y) acc -> (x * y) + acc) 0 (zip xs [l, l - 1 .. 1])
  where
    l = length xs
