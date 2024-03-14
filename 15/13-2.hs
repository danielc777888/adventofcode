-- 668

import Data.List
import qualified Data.Map as M

type Attendee = Char

type Arrangement = [Attendee]

type Happiness = M.Map (Attendee, Attendee) Int

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . maximum . map snd . totals . arrangements . lines

arrangements :: [String] -> ([Arrangement], Happiness)
arrangements xs = (ars, M.union hs hs')
  where
    as = nub $ map (\x -> ((words x) !! 0) !! 0) xs
    me = 'Z'
    ars = permutations (me : as)
    hs =
      M.fromList $
        map
          ( \x ->
              let ws = words x
                  a1 = (ws !! 0) !! 0
                  a2 = (ws !! 10) !! 0
                  d = (ws !! 2)
                  hu = read (ws !! 3) :: Int
               in ((a1, a2), if d == "gain" then hu else -hu)
          )
          xs
    hs' = M.fromList $ map (\x -> ((x, me), 0)) as ++ map (\x -> ((me, x), 0)) as

totals :: ([Arrangement], Happiness) -> [(Arrangement, Int)]
totals (xs, ys) = map (\x -> (x, total (x ++ [head x]) ys)) xs

total :: Arrangement -> Happiness -> Int
total (x : y : xs) ys = M.findWithDefault 0 (x, y) ys + M.findWithDefault 0 (y, x) ys + total (y : xs) ys
total _ _ = 0
