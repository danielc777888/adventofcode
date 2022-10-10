import Data.Maybe

main :: IO ()
main = interact solve

data Bag = Bag Int String [Bag] deriving (Show)

solve :: String -> String
solve = show . counts . containingBags "shinygold" . map (bag 1) . lines

-- solve = unlines . map show . containingBags "shinygold" . map (bag 1) . lines

bag :: Int -> String -> Bag
bag n xs = Bag n b bs
  where
    ws = words xs
    b = ws !! 0 ++ ws !! 1
    bs = bags (tail (dropWhile (/= "contain") ws))

bags :: [String] -> [Bag]
bags [] = []
bags (x : y : z : xs) = if x == "no" then [] else Bag (read x :: Int) (y ++ z) [] : bags (tail xs)

bags' :: Bag -> [Bag]
bags' (Bag _ _ bs) = bs

label :: Bag -> String
label (Bag _ l _) = l

containingBags :: String -> [Bag] -> [(Bag, Bool)]
containingBags l xs = containingBags' l xs xs

containingBags' :: String -> [Bag] -> [Bag] -> [(Bag, Bool)]
containingBags' _ _ [] = []
containingBags' l ys (x : xs) = if containsBag l ex then (x, True) : containingBags' l ys xs else (x, False) : containingBags' l ys xs
  where
    ex = expand x ys

expand :: Bag -> [Bag] -> [Bag]
expand b bs' =
  [b]
    ++ if null bs
      then []
      else
        concat $
          map
            ( \x ->
                let f = find x bs'
                 in if isJust f then expand (fromJust f) bs' else []
            )
            bs
  where
    bs = bags' b

counts :: [(Bag, Bool)] -> Int
counts xs = length (filter (\(_, y) -> y) xs) - 1

containsBag :: String -> [Bag] -> Bool
containsBag l bs = any (\b -> label b == l) bs

find :: Bag -> [Bag] -> Maybe Bag
find b [] = Nothing
find b (x : xs) = if label b == label x then Just x else find b xs
