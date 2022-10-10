import Data.Maybe

main :: IO ()
main = interact solve

data Bag = Bag Int String [Bag] deriving (Show)

solve :: String -> String
solve s = show $ counts b 1 ex
  where
    bs = map (bag 1) (lines s)
    b = fromJust (find (Bag 1 "shinygold" []) bs)
    ex = expand b bs

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

num :: Bag -> Int
num (Bag n _ _) = n

label :: Bag -> String
label (Bag _ l _) = l

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

counts :: Bag -> Int -> [Bag] -> Int
counts _ _ [] = 1
counts b n ys =
  n
    * sum
      ( map
          ( \c ->
              let d = fromJust (find c ys)
               in num c + counts d (num c) ys
          )
          bs
      )
  where
    bs = bags' b

sums :: [Bag] -> Int
sums = sum . map num

find :: Bag -> [Bag] -> Maybe Bag
find b [] = Nothing
find b (x : xs) = if label b == label x then Just x else find b xs
