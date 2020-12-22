
main :: IO()
main  = interact solve

data Exp = N Int
          | Add Exp Exp
          | Mult Exp Exp
          | Brk Exp deriving Show

--data Op = Add
  --      | Mult

solve :: String -> String
solve = show. sum . map (calc . parse) . lines

parse :: String -> Exp
parse = undefined

calc :: Exp ->  Int
calc (N x) = x
calc (Brk x) = calc x
calc (Add x y) = calc x + calc y
cacl (Mult x y) = calc x * calc y

