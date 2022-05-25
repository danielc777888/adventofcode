import qualified Data.Map as M

main :: IO()
main = interact solve

solve :: String -> String
solve s = show $ fst $ foldr (\x (s, m) -> let c = step 256 x in
                                         if M.member x m then (M.findWithDefault 0 x m + s, m) else (c + s, M.insert x c m)) (length is, M.empty) is
    where is = initialState s

initialState :: String -> [Int]
initialState [] = []
initialState xs = read ((takeWhile (/= ',') xs)) : initialState ys'
    where ys = dropWhile (/= ',') xs
          ys' = if null ys then [] else tail ys

step :: Int -> Int -> Int
step 0 _ = 0
step n x
    | x == 0 = 1 + step (n-1) 8 + step (n-1) 6
    | otherwise =  step (n-1) (x-1)


