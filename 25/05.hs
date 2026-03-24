

type Nat = Int

type Range = (Nat, Nat)

main :: IO ()
main = do
  putStrLn "--- Day 5: Cafeteria ---"
  c <- getContents
  let i = input c
  -- putStrLn (show i)
  putStrLn ("Part1: " <> show (part1 i))


input :: String -> ([Range], [Nat])
input s = (rs', is')
  where ls = lines s
        (rs, is) = break (== "") ls
        rs' = map (\r -> let (i1, i2) = break (== '-') r in
                            (read i1, read (tail i2))) rs
        is' = map read (tail is)

part1 :: ([Range], [Nat]) -> Nat
part1 (rs, is) = length (filter (\i -> any (\(i1, i2) -> i >= i1 && i <= i2) rs) is)
