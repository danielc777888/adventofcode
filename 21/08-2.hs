import Data.List
import Data.Maybe
import qualified Data.Set as S

type Digit = String

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . sum . map (value . match . signals) . lines

-- solve = show . map (match . signals) . lines

signals :: String -> ([Digit], [Digit])
signals s = (xs, ys)
  where
    ys = words $ drop 2 $ dropWhile (/= '|') s
    xs = words $ takeWhile (/= '|') s

match :: ([Digit], [Digit]) -> ([Digit], [Digit])
match (xs, ys) = ([zero, one, two, three, four, five, six, seven, eight, nine], ys)
  where
    (zero, sixes'') = mostSimilar sixes' one
    one = findPattern xs 2
    two = head fives''
    (three, fives') = mostSimilar fives one
    four = findPattern xs 4
    (five, fives'') = mostSimilar fives' four
    six = head sixes''
    seven = findPattern xs 3
    eight = findPattern xs 7
    (nine, sixes') = mostSimilar sixes three
    fives = filter (\x -> length x == 5) xs
    sixes = filter (\x -> length x == 6) xs
    findPattern xs l = fromJust $ find (\x -> length x == l) xs

value :: ([Digit], [Digit]) -> Int
value (xs, ys) = read $ concatMap (\y -> show (fromJust (elemIndex (sort y) xs'))) ys
  where
    xs' = map sort xs

mostSimilar :: [Digit] -> Digit -> (Digit, [Digit])
mostSimilar xs y = (snd (head ds), map snd (tail ds))
  where
    s = S.fromList y
    ds =
      sort $
        map
          ( \x ->
              let d = S.difference (S.fromList x) s
               in (S.size d, x)
          )
          xs
