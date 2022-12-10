import Data.Array
import Data.List

type Move = (Int, (Int, Int))
type Crates = Array Int [Crate]
type Crate = Char

main :: IO ()
main = do
  c <- getContents
  print $ part1 c
  print $ part2 c

part1 :: String -> [Crate]
part1 = topCrates . execute pickup9000 . procedure . lines

part2 :: String -> String
part2 = topCrates . execute pickup9001 . procedure . lines

procedure :: [String] -> ([Move], Crates)
procedure xs = (ms', cs'')
    where (cs, ms) = break (== "") xs
          ms' = map mkMove $ drop 1 ms
          cs' = map (dropWhile (==' ')) $ filter (\s -> not (null s || any (\c -> c == '[' || c == ']') s || last s == ' ')) $  transpose $ init cs
          cs'' = listArray (1, length cs') cs'

execute :: (Int -> [Crate] -> [Crate]) -> ([Move], Crates) -> Crates
execute pu ([], cs) = cs
execute pu ( (m:ms), cs) = execute pu (ms, cs')
    where (a, (b, c)) = m
          cs' = cs // [b', c']
          b' = (b, drop a (cs ! b))
          c' = (c, pu a (cs ! b) ++ (cs ! c))

pickup9000 :: Int -> [Crate] -> [Crate]
pickup9000 n cs = reverse $ take n cs

pickup9001 :: Int -> [Crate] -> [Crate]
pickup9001 n cs = take n cs

topCrates :: Crates -> [Crate]
topCrates = map head . elems

mkMove :: String -> Move
mkMove xs = case words xs of
    [_, a, _, b, _, c] -> (read a, (read b, read c))