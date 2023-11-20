{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe

main :: IO ()
main = BS.interact solve

solve :: BS.ByteString -> BS.ByteString
solve = BS.pack . show . sum .  map (fromIntegral . BS.length . expand) . divide

expand :: BS.ByteString -> BS.ByteString
expand xs
  | BS.null xs = BS.empty
  | BS.head xs == '(' = let (s1,s2) = BS.break (== ')') (BS.drop 1 xs)
                            (t,r) = marker s1
                            t' = fromIntegral t
                            r' = fromIntegral r
                            ex = BS.concat (replicate r' (BS.take t' (BS.tail s2)))
                            xs' = BS.drop t' (BS.tail s2)
                        in BS.append (expand ex) (expand xs')
  | otherwise = BS.cons (BS.head xs) (BS.tail xs)


marker :: BS.ByteString -> (Int,Int)
marker xs = case (BS.words (BS.map (\c -> if c == 'x' then ' ' else c) xs)) of
  [x,y] -> (fst (fromJust (BS.readInt x)), fst (fromJust (BS.readInt y)))
  _ -> error $ "marker: Unrecognized pattern " ++ (BS.unpack xs)

divide :: BS.ByteString -> [BS.ByteString]
divide xs
  | BS.null xs = []
  | BS.head xs == '(' = let (s1,s2) = BS.break (== ')') (BS.drop 1 xs)
                            (t,r) = marker s1
                            t' = fromIntegral t
                        in
                        BS.concat ["(",s1,")",BS.take t' (BS.drop 1 s2)]: divide (BS.drop (t'+1) s2)
  | otherwise = let (s1,s2) = BS.break (== '(') xs
                in
                  s1:divide s2
