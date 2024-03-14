-- 258

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . length . filter nice . lines

nice :: String -> Bool
nice xs = if (hasVowels xs && hasRepeatedLetter xs && not (hasSpecialLetters xs)) then True else False

hasVowels :: String -> Bool
hasVowels xs = length (filter (\x -> x == 'a' || x == 'e' || x == 'i' || x == 'o' || x == 'u') xs) >= 3

hasRepeatedLetter :: String -> Bool
hasRepeatedLetter xs = length (filter (\(x, y) -> x == y) (zip xs (tail xs))) >= 1

hasSpecialLetters :: String -> Bool
hasSpecialLetters xs = length (filter (\x -> x == ('a', 'b') || x == ('c', 'd') || x == ('p', 'q') || x == ('x', 'y')) (zip xs (tail xs))) >= 1
