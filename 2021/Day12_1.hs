import Data.Maybe
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
    -- | otherwise = M.insertWith (++) a [b] $ M.insertWith (++) b [a] m
    | otherwise = M.insertWith (++) a [b] m
    where (a, b) = (takeWhile (/='-') xs, drop 1 (dropWhile (/='-') xs))

mkVertex :: String -> Vertex
mkVertex xs
    | xs == "start" = Start []
    | xs == "end" = End
    | otherwise = Vertex xs []


glue :: M.Map String [String] -> Vertex
glue m = Start vs'
    where vs = fromJust (M.lookup "start" m)
          vs' = map (glue' m) vs

glue' :: M.Map String [String] -> String -> Vertex
glue' m "end" = End
glue' m x = Vertex x vs'
     where vs = fromMaybe [] (M.lookup x m)
           vs' = map (glue' m) vs

paths :: Vertex -> [Path]
paths = undefined


