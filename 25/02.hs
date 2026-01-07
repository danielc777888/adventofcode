{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TI

type Range = (Integer, Integer)

main :: IO ()
main = do
  putStrLn "2025 -- Day 2 -- Gift Shop"
  contents <- TI.getContents
  let ranges = parse contents
  print ranges
  putStrLn ("Part 1: " <> show (part1 ranges))
  putStrLn ("Part 2: " <> show (part2 contents))


parse :: T.Text -> [Range]
parse = map (\y -> let [f, t] = T.splitOn "-" y 
                       in (read (T.unpack f), read (T.unpack t))) .  (T.splitOn ",")

-- 
part1 :: [Range] -> Integer
part1 = (foldr (+) 0) . concatMap (\(x, y) -> filter invalid [x .. y])

-- 
part2 :: T.Text -> T.Text
part2 = id

invalid :: Integer -> Bool
invalid x = True
