import Data.Char
import Data.Aeson
import Data.Aeson.KeyMap (elems)
import Data.Maybe
import qualified Data.ByteString as BS
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import Data.Scientific
import qualified Data.Vector as V

main :: IO ()
main = BS.interact solve

solve :: BS.ByteString -> BS.ByteString
solve = encodeUtf8 . T.pack . show . countNumbers . fromJust . decodeStrict

countNumbers :: Value -> Int
countNumbers (Object o)  = if any (hasString "red") (elems o) then 0 else sum $ map countNumbers (elems o)
countNumbers (Number n) = fromIntegral $ coefficient n
countNumbers (Array v) = sum $ V.map countNumbers v
countNumbers _  = 0

hasString :: String -> Value -> Bool
hasString s (String t) = T.unpack t == s
hasString _ _ = False




