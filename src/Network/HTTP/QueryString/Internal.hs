module Network.HTTP.QueryString.Internal where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC
import           Data.List             (sort)
import           Data.Map              (Map)
import qualified Data.Map              as Map
import qualified Network.HTTP.Types    as HTTP

-- | A query string for HTTP.
--
-- > "param1=value1&param2=value2"
data QueryString = QueryString
    { rawData :: [(ByteString, ByteString)]
    }
    deriving (Eq)

instance Semigroup QueryString where
  a <> b = QueryString $ rawData a `merge` rawData b

instance Monoid QueryString where
    mempty = QueryString []
    mappend a b = QueryString $ rawData a `merge` rawData b

merge :: Ord a => [a] -> [a] -> [a]
merge [] bs = bs
merge as [] = as
merge (a:as) (b:bs)
    | a <= b    = a:merge as (b:bs)
    | otherwise = b:merge (a:as) bs

instance Read QueryString where
    readsPrec _
        = maybe [] (\a -> [(a, "")])
        . parseQuery
        . BC.pack

parseQuery :: ByteString -> Maybe QueryString
parseQuery
    = fmap queryString
    . sequence
    . map (t . map (HTTP.urlDecode True) . BC.split '=')
    . BC.split '&'
  where
    t [a, b] = Just (a, b)
    t _      = Nothing

toString :: QueryString -> ByteString
toString = BS.intercalate "&" . map concatWithEqual . rawData
  where
    concatWithEqual ("", _)    = error "name is null."
    concatWithEqual (key, val) = mconcat
        [ HTTP.urlEncode True key
        , "="
        , HTTP.urlEncode True val
        ]

-- | Convert a parameter list to 'QueryString'.
--
-- >>> toString $ queryString [("param1", "value1"), ("param2", "value2")]
-- "param1=value1&param2=value2"
queryString :: [(ByteString, ByteString)] -> QueryString
queryString = QueryString . sort

-- | Convert a parameter map to 'QueryString'.
--
-- >>> import qualified Data.Map as Map
-- >>> toString $ queryStringFromMap $ Map.fromList [("param1", "value1"), ("param2", "value2")]
-- "param1=value1&param2=value2"
queryStringFromMap :: Map ByteString ByteString -> QueryString
queryStringFromMap = queryString . Map.toList
