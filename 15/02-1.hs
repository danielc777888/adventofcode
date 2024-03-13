-- 1598415

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . sum . wrappingPaper . boxes . lines

data Box = Box Length Width Height deriving (Show)

type Length = Int

type Width = Int

type Height = Int

length :: Box -> Length
length (Box l _ _) = l

width :: Box -> Width
width (Box _ w _) = w

height :: Box -> Height
height (Box _ _ h) = h

boxes :: [String] -> [Box]
boxes = map (\x -> box x [])

box :: String -> [String] -> Box
box [] b = Box l w h
  where
    l = read (b !! 0)
    w = read (b !! 1)
    h = read (b !! 2)
box xs b = box ys (b ++ [n])
  where
    n = takeWhile (\x -> x /= 'x') xs
    ys = safeTail (dropWhile (\x -> x /= 'x') xs)

safeTail :: [a] -> [a]
safeTail []       = []
safeTail (x : xs) = xs

wrappingPaper :: [Box] -> [Int]
wrappingPaper = map (\b -> (surfaceArea b) + areaSmallestSide b)

surfaceArea :: Box -> Int
surfaceArea b = (2 * l * w) + (2 * w * h) + (2 * h * l)
  where
    l = Main.length b
    w = Main.width b
    h = Main.height b

areaSmallestSide :: Box -> Int
areaSmallestSide b = minimum [l * w, w * h, h * l]
  where
    l = Main.length b
    w = Main.width b
    h = Main.height b
