import Data.Char
import Data.Maybe

data FishNum
  = Pair FishNum FishNum
  | Regular (Int, Int)
  deriving (Show)

data Action
  = Explosion ((Int, Int), (Int, Int))
  | Split

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . magnitude . reduce . map mkFish . lines

mkFish :: String -> FishNum
mkFish (x : xs)
  | x == '[' = Pair (mkFish ln) (mkFish rn)
  | x == ']' = mkFish xs
  | isDigit x = Regular (0, r)
  where
    (ln, rn) = splitNum (x : xs)
    r = read $ x : takeWhile isDigit xs

splitNum :: String -> (String, String)
splitNum xs = (drop 1 ls, init (drop 1 rs))
  where
    n = bestSplit xs
    (ls, rs) = splitAt n xs

bestSplit :: String -> Int
bestSplit xs = snd $ minimum $ commaMunch xs (0, 0)

commaMunch :: String -> (Int, Int) -> [(Int, Int)]
commaMunch [] _ = []
commaMunch (x : xs) (m, i)
  | x == '[' = commaMunch xs (m + 1, i + 1)
  | x == ']' = commaMunch xs (m - 1, i + 1)
  | x == ',' = (m, i) : commaMunch xs (m, i + 1)
  | otherwise = commaMunch xs (m, i + 1)

reduce :: [FishNum] -> FishNum
reduce xs = foldr (\x acc -> reduce' (Pair acc x)) (head xs') (drop 1 xs')
  where
    xs' = reverse xs

reduce' :: FishNum -> FishNum
reduce' x = x''
  where
    (x'', _, _) = (until done step x')
    x' = step (x, Nothing, Nothing)

step :: (FishNum, Maybe Action, Maybe Action) -> (FishNum, Maybe Action, Maybe Action)
step (x, Nothing, Nothing) = undefined

step' :: FishNum -> FishNum
step' (Pair l@(Regular (l1, l2)) r@(Regular (r1, r2)))
  | l2 >= 10 && r2 >= 10 = Pair (Regular (l1, l2 `div` 2) (Regular (r1, r2 `div` 2)))
  | l2 >= 10 = Pair (Regular (l1, l2 `div` 2) r)
  | r2 >= 10 = Pair l (Regular (r1, r2 `div` 2))
  | otherwise = Pair l r
step' (Pair l r) = Pair (step' l) (step' r)

done :: (FishNum, Maybe Action, Maybe Action) -> Bool
done (_, Nothing, Nothing) = True
done _ = False

magnitude :: FishNum -> Int
magnitude (Pair l r) = (3 * magnitude l) + (2 * magnitude r)
magnitude (Regular (_, y)) = y

showFish :: FishNum -> String
showFish (Pair l r) = "[" ++ showFish l ++ "," ++ showFish r ++ "]"
showFish (Regular (x, y)) = show y
