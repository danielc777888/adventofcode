import Aoc.Loop (apply)

type FloorNum = Int
type Id = Int

data E = E FloorNum Bool deriving Show

data Comp = G Id
  | M Id deriving (Show,Eq)

data Sim = Sim {
  step :: Int,
  elev :: E,
  floors :: [[Comp]]
  } deriving Show

example :: Sim
example = Sim 0 (E 0 True) [
  [(M 0),(M 1)],
  [(G 0)],
  [(G 1)],
  []]

main :: IO ()
main = interact solve

solve :: String -> String
solve _ = show (apply 10 run example)

run :: Sim -> Sim
run s@(Sim n (E fn u) fs)
  | complete s = s
  | otherwise  = (Sim (n+1) e' fs')
  where (e',cs') = case fn of
          0 -> (E (fn+1) u, take 2 (filter (\c -> canMove c f0 f1) f0))
        fs' = fs
        f0 = fs!!0
        f1 = fs!!1
        f2 = fs!!2
        f3 = fs!!3

complete :: Sim -> Bool
complete s = length ((floors s)!!3) == l
  where l = length (concat (floors s))

canMove :: Comp -> [Comp] -> [Comp] -> Bool
canMove g@(G i) f t = if anyM i f then False else True
canMove m@(M i) f t = if not (anyG i f) && anyOtherG i f then False else True

anyM :: Id -> [Comp] -> Bool
anyM _ [] = False
anyM n (x:xs) = case x of
  (M id) -> if id == n then True else anyM n xs
  _ -> anyM n xs

anyG :: Id -> [Comp] -> Bool
anyG _ [] = False
anyG n (x:xs) = case x of
  (G id) -> if id == n then True else anyG n xs
  _ -> anyG n xs

anyOtherG :: Id -> [Comp] -> Bool
anyOtherG _ [] = False
anyOtherG n (x:xs) = case x of
  (G id) -> if id /= n then True else anyOtherG n xs
  _ -> anyOtherG n xs
