import Data.List
import Data.Map qualified as M

type Attendee = Char

type Arrangement = [Attendee]

type Happiness = M.Map (Attendee, Attendee) Int

main :: IO ()
main = interact solve

solve :: String -> String
-- solve = show . minimum . totals . arrangements . lines
solve = show . maximum . map snd . totals . arrangements . lines

arrangements :: [String] -> ([Arrangement], Happiness)
arrangements xs = (ars, hs)
  where
    as = nub $ map (\x -> ((words x) !! 0) !! 0) xs
    ars = permutations as
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

totals :: ([Arrangement], Happiness) -> [(Arrangement, Int)]
totals (xs, ys) = map (\x -> (x, total (x ++ [head x]) ys)) xs

total :: Arrangement -> Happiness -> Int
total (x : y : xs) ys = M.findWithDefault 0 (x, y) ys + M.findWithDefault 0 (y, x) ys + total (y : xs) ys
total _ _ = 0
