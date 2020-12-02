
main :: IO()
main = interact solve

data Policy = Policy Range Char Password deriving Show
type Range = (Int, Int)
type Password = String

solve :: String -> String
solve  = show . length  . filter (== True) .  map (valid . policy) . lines

policy :: String -> Policy
policy xs = Policy range letter password
  where letter = head (ws!!1)
        password = ws!!2
        range = (read (fst ys)::Int, read (drop 1 (snd ys))::Int)
        ws = words xs
        ys = break (=='-') (ws!!0)

valid :: Policy -> Bool
valid (Policy r l p)  = (p1 == l && p2 /= l) || (p2 == l && p1 /= l)
  where p1 = p!!((fst r)-1)
        p2 = p!!((snd r)-1)


