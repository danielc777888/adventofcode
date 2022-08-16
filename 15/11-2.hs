import Data.Char

main :: IO ()
main = interact (solve . increment . solve)

type Password = String

solve :: String -> String
solve = (until valid increment) . head . lines

increment :: Password -> Password
increment xs = increment' xs []

increment' :: Password -> [Char] -> Password
increment' [] xs = xs
increment' xs ys = if (not w) then init xs ++ [x] ++ ys else increment' (init xs) (x : ys)
  where
    (x, w) = incChar (last xs)

incChar :: Char -> (Char, Bool)
incChar x = if (ord x) `mod` 122 == 0 then ('a', True) else (chr ((ord x) + 1), False)

valid :: Password -> Bool
valid x = length x == 9 || (increasing x && pairs x [] 0 && letters x)

increasing :: Password -> Bool
increasing (x : y : z : xs) = if y' - x' == 1 && z' - y' == 1 then True else increasing (y : z : xs)
  where
    x' = ord x
    y' = ord y
    z' = ord z
increasing _ = False

pairs :: Password -> [String] -> Int -> Bool
pairs [] _ _ = False
pairs [x] _ _ = False
pairs (x : y : xs) ys n
  | x == y && n == 1 && [x, y] `notElem` ys = True
  | x == y = pairs xs ([[x, y]] ++ ys) (n + 1)
  | otherwise = pairs (y : xs) ys n

letters :: Password -> Bool
letters xs = 'i' `notElem` xs && 'o' `notElem` xs && 'l' `notElem` xs
