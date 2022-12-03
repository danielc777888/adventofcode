{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Aeson.KeyMap (elems)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BTC
import Data.Maybe
import Data.Scientific

main :: IO ()
main = interact solve

solve :: String -> String
solve = show . countNumbers . fromJust . decodeStrict . BTC.pack

countNumbers :: Value -> Int
countNumbers (Object o) = if any (== String "red") (elems o) then 0 else sum $ map countNumbers (elems o)
countNumbers (Number n) = fromIntegral $ coefficient n
countNumbers (Array v) = sum $ fmap countNumbers v
countNumbers _ = 0
