main :: IO ()
main = interact solve

solve :: String -> String
solve = show . length . spawn 80 . initialState

initialState :: String -> [Int]
initialState [] = []
initialState xs = read ((takeWhile (/= ',') xs)) : initialState ys'
  where
    ys = dropWhile (/= ',') xs
    ys' = if null ys then [] else tail ys

spawn :: Int -> [Int] -> [Int]
spawn 0 xs = xs
spawn n xs = spawn (n - 1) (step xs)

step :: [Int] -> [Int]
step [] = []
step (x : xs)
  | x == 0 = 6 : 8 : step xs
  | otherwise = (x - 1) : step xs
