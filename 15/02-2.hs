-- 3812909

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . sum . ribbon . boxes . lines

data Box = Box
  { length :: Int,
    width  :: Int,
    height :: Int
  }
  deriving (Show)

boxes :: [String] -> [Box]
boxes = map (\x -> box x [])

box :: String -> [String] -> Box
box [] b = Box {Main.length = l, width = w, height = h}
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

ribbon :: [Box] -> [Int]
ribbon = map (\b -> ribbonWrap b + ribbonBow b)

ribbonWrap :: Box -> Int
ribbonWrap (Box l w h) = minimum [2 * l + 2 * w, 2 * l + 2 * h, 2 * w + 2 * h]

ribbonBow :: Box -> Int
ribbonBow (Box l w h) = l * w * h
