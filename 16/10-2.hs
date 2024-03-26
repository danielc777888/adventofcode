-- 55637

import Data.List
import qualified Data.Map as M
import Data.Maybe

type Bot = (Int,(Int,Int))
type Output = (Int,Int)

data Instruction =
  ValueI (Int,Int)
  | BotI (Int, (Sub,Sub))
  deriving Show

data Sub = OutputS Int
  | BotS Int
  deriving Show

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . product . map snd . take 3 . sort . snd . run . map instruction . lines

run :: [Instruction] -> ([Bot],[Output])
run xs = snd (until (M.null . fst) (run' xs) (pendingBots xs,([],[])))

run' :: [Instruction] -> (M.Map Int (Int,Int), ([Bot],[Output])) -> (M.Map Int (Int,Int), ([Bot],[Output]))
run' [] bm = bm
run' (x:xs) m@(bm, (bs, os)) =
  case x of
  (ValueI (v,k)) -> if M.notMember k bm then run' xs m else run' xs ((updateBot k v bm), (bs,os))
  (BotI (k, (BotS k2, BotS k3))) ->
    let bm'= updateBot k2 l (M.delete k bm)
        bm'' = updateBot k3 h bm'
        b@(t1,t2) = fromJust (M.lookup k bm)
        (l,h) = (min t1 t2, max t1 t2) in
    if M.member k bm && done b then  run' xs (bm'', ((k,(l,h)):bs,os)) else run' xs m
  (BotI (k, (BotS k2, OutputS o1))) ->
    let bm'= updateBot k2 l (M.delete k bm)
        b@(t1,t2) = fromJust (M.lookup k bm)
        (l,h) = (min t1 t2, max t1 t2) in
    if M.member k bm && done b then run' xs (bm', ((k,(l,h)):bs,(o1,h):os)) else run' xs m
  (BotI (k, (OutputS o1, BotS k2))) ->
    let bm'= updateBot k2 h (M.delete k bm)
        b@(t1,t2) = fromJust (M.lookup k bm)
        (l,h) = (min t1 t2, max t1 t2) in
    if M.member k bm && done b then run' xs (bm', ((k,(l,h)):bs,(o1,l):os)) else run' xs m
  (BotI (k, (OutputS o1, OutputS o2))) ->
    let bm' = M.delete k bm
        b@(t1,t2) = fromJust (M.lookup k bm)
        (l,h) = (min t1 t2, max t1 t2) in
      if M.member k bm && done b then run' xs (bm', ((k,(l,h)):bs, (o1,l):(o2,h):os)) else run' xs m
  _ -> error "Unrecognized pattern"

updateBot :: Int -> Int -> M.Map Int (Int,Int) -> M.Map Int (Int,Int)
updateBot b x bm = M.update (\t -> case t of
                                     (-1,-1) -> Just (x,-1)
                                     (t1,t2) -> Just (t1,if x == t1 then t2 else x)
                                     _ -> Just t
                            ) b bm

done :: (Int,Int) -> Bool
done (x,y) = x /= -1 && y /= -1

pendingBots :: [Instruction] -> M.Map Int (Int,Int)
pendingBots xs = foldr insertBot M.empty xs

insertBot :: Instruction -> M.Map Int (Int,Int) -> M.Map Int (Int,Int)
insertBot is bm = case is of
  (ValueI (_,y)) -> M.insert y (-1,-1) bm
  (BotI (x,_))   -> M.insert x (-1,-1) bm


instruction :: String -> Instruction
instruction xs = case filter (`notElem` ["goes","to","gives","low","and","high"]) (words xs) of
  ["value",x,_,y]   -> ValueI (read x, read y)
  ["bot",x,l,y,h,z] -> BotI (read x, (sub l (read y), sub h (read z)))

sub :: String -> Int -> Sub
sub "output" x = OutputS x
sub "bot" x    = BotS x
