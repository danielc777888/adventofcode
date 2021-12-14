import qualified Data.Map as M
import Data.Maybe

data Instruction = FoldUp Int
    | FoldLeft Int deriving Show

type Paper = M.Map (Int, Int) Char

main :: IO()
main = interact solve

solve :: String -> String
solve = showCode . foldPaper . instructions . lines

instructions :: [String] -> (Paper, [Instruction])
instructions xs = (mkPaper p, mkInstructions is)
    where p = takeWhile (not . null) xs
          is = drop 1 (dropWhile (not . null) xs)

mkPaper :: [String] -> Paper
mkPaper xs = foldr (\c acc -> M.insert c '#' acc) p xs'
    where xs' = map (\x -> (read (takeWhile (/=',') x), read (drop 1 (dropWhile (/=',') x )))) xs
          mx = maximum (map fst xs')
          my = maximum (map snd xs')
          p = M.fromList [((x, y), '.') | x <- [0..mx], y <- [0..my]]


mkInstructions :: [String] -> [Instruction]
mkInstructions = map (\x -> let is = (words x)!!2
                                is' = head is
                                v = read (drop 1 (dropWhile (/='=') is)) in
                                if is' == 'y' then FoldUp v else FoldLeft v
                     )

foldPaper :: (Paper, [Instruction]) -> Paper
foldPaper (p, is) = foldr(\i q -> M.foldrWithKey(\(x, y) v xs -> update i xs ((x,y),v) ) M.empty q) p (reverse is)

update :: Instruction -> Paper -> ((Int, Int), Char) -> Paper
update (FoldLeft v) p ((x, y), c)
    | x > v && c == '#' = M.insert (x - ((x - v) * 2), y) '#' p
    | x >= v = p
    | otherwise = M.insertWith (\n o -> '#') (x, y) c p
update (FoldUp v) p ((x, y), c)
    | y > v && c == '#' = M.insert (x, y - ((y - v) * 2)) '#' p
    | y >= v = p
    | otherwise = M.insertWith (\n o -> '#') (x, y) c p

showCode :: Paper -> String
showCode p = unlines $ map (\y -> [fromJust (M.lookup (x, y) p) | x <- [0..mx]] ) [0..my]
    where ((mx, my),_) = M.findMax p
