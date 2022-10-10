-- TODO Takes too long, cannot find solution after 1 H

main :: IO ()
main = interact solve

type PublicKey = Int

type LoopSize = Int

type SubjectNumber = Int

solve :: String -> String
solve = show . encryptionKeys . map loopSize . map read . lines

loopSize :: PublicKey -> (PublicKey, LoopSize)
loopSize x = head [(x, y) | y <- [1 .. 1000000], isLoopSize x y 1]

isLoopSize :: PublicKey -> LoopSize -> Int -> Bool
isLoopSize x y z
  | y == 0 = False
  | x == z' = True
  | otherwise = isLoopSize x (y - 1) z'
  where
    z' = transform z 7

transform :: Int -> SubjectNumber -> Int
transform x y = (x * y) `mod` 20201227

encryptionKeys :: [(PublicKey, LoopSize)] -> [Int]
encryptionKeys ((x, y) : (x', y') : []) = encryptionKey x y' 1 : encryptionKey x' y 1 : []

encryptionKey :: PublicKey -> LoopSize -> Int -> Int
encryptionKey x 0 z = z
encryptionKey x y z = encryptionKey x (y - 1) (transform z x)
