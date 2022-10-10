import Data.Array
import Data.Char

type Path = ([Vertex], Distance)

type State = (Links, [Vertex])

type Links = Array Vertex (Vertex, Distance)

type Distance = Int

type Graph = ([Vertex], [Edge])

type AdjArray = Array Vertex [(Vertex, Weight)]

type Edge = (Vertex, Vertex, Weight)

type Vertex = Int

type Weight = Int

type Weights = Array (Vertex, Vertex) Weight

type Nat = Int

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . risk . dijkstra . mkGraph . lines

risk :: Path -> Int
risk (vs, d) = d

mkGraph :: [String] -> (Graph, Vertex)
mkGraph xs = (toGraph adj, v)
  where
    v = length xs'
    xs' = concat xs
    cl = length (head xs)
    ws = listArray (1, length xs') (map digitToInt xs')
    adj = mkAdjArray v cl [1 .. (length xs')] ws

mkAdjArray :: Int -> Int -> [Vertex] -> Array Vertex Weight -> AdjArray
mkAdjArray vl cl vs arr = listArray (1, vl) es
  where
    es =
      map
        ( \v ->
            let r = if v `mod` cl == 0 then [] else [(rv, arr ! rv)]
                b = if bv > vl then [] else [(bv, arr ! bv)]
                t = if tv < 1 then [] else [(tv, arr ! tv)]
                l = if v <= 1 || lv `mod` cl == 0 then [] else [(lv, arr ! lv)]
                rv = v + 1
                lv = v - 1
                bv = v + cl
                tv = v - cl
             in b ++ r ++ l
        )
        vs

toGraph :: AdjArray -> Graph
toGraph a = (indices a, [(u, v, w) | (u, vws) <- assocs a, (v, w) <- vws])

dijkstra :: (Graph, Vertex) -> Path
dijkstra (g, v) = path (until done (gstep wa) (start n))
  where
    path (ls, vs) = (reverse (getPath ls v), distance ls v)
    done (ls, vs) = v `notElem` vs
    n = length (nodes g)
    wa = weights g

gstep :: Weights -> State -> State
gstep wa (ls, vs) = (ls', vs')
  where
    (d, v) = minimum [(distance ls v, v) | v <- vs]
    vs' = filter (/= v) vs
    ls' = accum better ls [(u, (v, sum d (wa ! (v, u)))) | u <- vs']
      where
        sum d w = if w == maxInt then maxInt else d + w
    better (v1, d1) (v2, d2) = if d1 <= d2 then (v1, d1) else (v2, d2)

maxInt :: Int
maxInt = maxBound

start :: Nat -> State
start n = (array (1, n) ((1, (1, 0)) : [(v, (v, maxInt)) | v <- [2 .. n]]), [1 .. n])

getPath :: Links -> Vertex -> [Vertex]
getPath ls v = if u == v then [u] else v : getPath ls u
  where
    u = parent ls v

weights :: Graph -> Array (Vertex, Vertex) Weight
weights g = listArray ((1, 1), (n, n)) (repeat maxInt) // [((u, v), w) | (u, v, w) <- edges g]
  where
    n = length (nodes g)

parent :: Links -> Vertex -> Vertex
parent ls v = fst (ls ! v)

distance :: Links -> Vertex -> Vertex
distance ls v = snd (ls ! v)

nodes :: ([Vertex], [Edge]) -> [Vertex]
nodes (vs, es) = vs

edges :: ([Vertex], [Edge]) -> [Edge]
edges (vs, es) = es

source :: Edge -> Vertex
source (u, v, w) = u

target :: Edge -> Vertex
target (u, v, w) = v

weight :: Edge -> Weight
weight (u, v, w) = w
