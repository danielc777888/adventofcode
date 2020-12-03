
main :: IO()
main = interact solve

type Area = [Row]
type Row = [Location]
data Location = Open Position
                | Tree Position deriving (Eq, Show)
type Position = (Int, Int)

data Slope = RightDown Int Int

solve :: String -> String
solve xs =  show (product (map (\s -> treesEncountered (traverseArea s (0,0) [] a)) slopes))
  where a = area (lines xs)
  
--solve =  unlines . map show .  area . lines

slopes :: [Slope]
slopes = [RightDown 1 1, RightDown 3 1, RightDown 5 1, RightDown 7 1, RightDown 1 2]

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


