
import qualified Data.Map as M

data Vertex  = Start [Vertex]
            | Vertex String [Vertex]
            | End deriving Show


type Path = [Vertex]

main :: IO()
main = interact solve

solve :: String -> String
solve = show . mkGraph . lines

mkGraph :: [String] -> Vertex
mkGraph xs = glue m
    where m = foldr insertNode M.empty xs

insertNode :: String -> M.Map String [String] -> M.Map String [String]
insertNode xs m
    | a == "start" = M.insertWith (++) a [b] m
    | b == "start" = M.insertWith (++) b [a] m
    | a == "end" = M.insertWith (++) b [a] m
    | b == "end" = M.insertWith (++) a [b] m
    | otherwise = M.insertWith (++) a [b] $ M.insertWith (++) b [a] m
    where (a, b) = (takeWhile (/='-') xs, drop 1 (dropWhile (/='-') xs))

mkVertex :: String -> Vertex
mkVertex xs
    | xs == "start" = Start []
    | xs == "end" = End
    | otherwise = Vertex xs []


glue :: M.Map String [String] -> Vertex
glue m =

paths :: Vertex -> [Path]
paths = undefined


