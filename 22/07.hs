import Data.Maybe
import Data.List

data Node = Root [Child]
    | Dir Name Size Parent [Child]
    | File Name Size Parent
    deriving (Eq, Show)

type Parent = Node
type Child = Node
type Size = Int
type Name = String

main :: IO ()
main = do
  c <- getContents
  print $ part1 c

-- print $ part2 c

part1 :: String -> Node
part1 = mkTree . lines

part2 :: String -> String
part2 = id

mkTree :: [String] -> Node
mkTree xs = parse (Root []) (drop 1 xs)

parse :: Node -> [String] -> Node
parse n []  = n
parse n (x:xs) = case words x of
    ["$", "ls"] -> parse n' xs'
    ["$", "cd", ".."] -> parse (parent n) xs
    ["$", "cd", d] -> parse (child n d) xs
    _ -> error "some ting is not lekka...."
    where (n', xs') = parseContents n xs

parseContents :: Node -> [String] -> (Node, [String])
parseContents n xs = (mkChildren n cs, xs')
    where (cs, xs') = break ("$" `isPrefixOf`) xs

child :: Node -> String -> Node
child nd s = case nd of
    (Root cs) -> f s cs
    (Dir _ _ _ cs) -> f s cs
    where f nm xs = fromJust $ find (\x -> name x == nm) xs

name :: Node -> Name
name (Dir n _ _ _) = n
name (File n _ _) = n

size :: Node -> Size
size (Dir _ s _ _) = s
size (File _ s _) = s

parent :: Node -> Parent
parent (Dir _ _ p _) = p
parent (File _ _ p) = p

mkChildren :: Node -> [String] -> Node
mkChildren nd xs = case nd of
    (Root _) -> r'
    (Dir n s p cs) -> d'
    where
        r' = Root (map (mkChild  nd) xs)
        d' = Dir (name nd) (size nd) (parent nd) (map (mkChild  nd) xs)

mkChild :: Node -> String -> Node
mkChild p s = case words s of
    ["dir", n] -> Dir n 0 p []
    [s, n] -> File n (read s) p
    _ -> error "making of child gone awry"
