-- 241

import Data.Function
import Data.List
import qualified Data.Map as M

type Compounds = M.Map String Int

data Sue = Sue
  { num       :: Int,
    diffValue :: Int,
    compounds :: Compounds
  }
  deriving (Show)

main :: IO ()
main = interact $ show . solve

solve :: String -> String
solve = show . minimumBy (compare `on` diffValue) . map (diff' analysis . sue) . lines

analysis :: Compounds
analysis =
  M.fromList
    [ ("children", 3),
      ("cats", 7),
      ("samoyeds", 2),
      ("pomeranians", 3),
      ("akitas", 0),
      ("vizslas", 0),
      ("goldfish", 5),
      ("trees", 3),
      ("cars", 2),
      ("perfumes", 1)
    ]

sue :: String -> Sue
sue x = case words x of
  [_, n, k1, v1, k2, v2, k3, v3] -> Sue (read (init n)) 0 (M.fromList [(init k1, read (init v1)), (init k2, read (init v2)), (init k3, read v3)])

diff :: Compounds -> Sue -> Sue
diff m (Sue n _ sc) = Sue n d sc
  where
    d = sum $ M.elems $ M.intersectionWithKey (\k a b -> abs (subtract b a)) sc m

diff' :: Compounds -> Sue -> Sue
diff' m (Sue n _ sc) = Sue n d sc
  where
    d = sum $ M.elems $ M.intersectionWithKey f sc m
    f k v1 v2
      | (k == "cats" || k == "trees") = if v1 > v2 then 0 else (v2 - v1) + 1
      | (k == "goldfish" || k == "pomeranians") = if v1 < v2 then 0 else (v1 - v2) + 1
      | otherwise = abs (subtract v2 v1)
