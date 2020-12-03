
main :: IO()
main = interact solve

type Area = [Row]
type Row = [Location]
data Location = Open Position
                | Tree Position deriving (Eq, Show)
type Position = (Int, Int)

data Slope = RightDown Int Int

solve :: String -> String
solve =  show . treesEncountered . traverseArea (RightDown 3 1) (0,0) [] . area . lines
--solve =  unlines . map show .  area . lines

area :: [String] -> Area
area xs = map row (zip xs [0..])

row :: (String, Int) -> Row
row (xs, x) = map (location x) (zip xs [0..]) 

location ::  Int -> (Char, Int) -> Location
location x (y, z) = if y == '.' then Open (x, z) else Tree (x, z)

               
traverseArea :: Slope -> Position -> [Position] -> Area -> (Area, [Position])
traverseArea (RightDown r d) (x, y) ps a = if x >=  (length a) - 1 then (a , ps) else traverseArea (RightDown r d) np (ps ++ [np]) a
                                         where np = (x + d, y + r)

treesEncountered :: (Area, [Position]) -> Int
treesEncountered (a, ps)  = length $ filter (==True) $ map (\p -> isTreeLocation a p ) ps

isTreeLocation :: Area -> Position -> Bool
isTreeLocation a (x, y) = length [l | r <- a, l <- r,  (x, y `mod` colCount) == position l && isTree l] > 0
                          where colCount = length (head a)


isTree :: Location -> Bool
isTree (Tree _) = True
isTree _ = False


position :: Location  -> Position
position (Tree  p) = p
position (Open  p) = p


