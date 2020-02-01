{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Network.DO.Net.Common where

import           Control.Comonad.Env.Class (ComonadEnv, ask)
import           Data.Aeson                as A hiding (Result, (<?>))
import qualified Data.Aeson.Types          as A
import qualified Data.ByteString.Char8     as B8
import qualified Data.HashMap.Strict       as H
import           Data.Maybe
import           Data.Proxy
import           Data.Text                 (Text)
import qualified Data.Vector               as V
import           Network.DO.Types          as DO hiding (URI, error)
import           Network.HTTP.QueryString  (QueryString)
import qualified Network.HTTP.QueryString  as QS
import           Network.REST
import           Network.URI               (URI, parseURI)
import           Prelude                   as P

rootURI :: String
rootURI = "https://api.digitalocean.com"

apiVersion ::  String
apiVersion = "v2"

(</>) :: String -> String -> String
s </> ('/': s') = s ++ s'
s </> s'        = s ++ "/" ++ s'

(<?>) :: String -> String -> String
s <?> ('?': s') = s ++ s'
s <?> s'        = s ++ "?" ++ s'

toURI :: String -> URI
toURI s = maybe (P.error $ "cannot parse URI from " ++ s) id $ parseURI s

toList :: (FromJSON a) => Text -> Value -> Result [a]
toList k (Object o) = if H.member "message" o
  then error . show $ o H.! "message"
  else case H.lookup k o of
    Just (Array boxes) ->
      Right $ mapMaybe (A.parseMaybe parseJSON) (V.toList boxes)
    _ ->
      error $ "cannot decode JSON value to a list of " ++ show k

toList k  _         =
  error $ "cannot decode JSON value to a list of " ++ show k

class Listable a where
  listEndpoint :: Proxy a -> String
  listField :: Proxy a -> Text

queryList :: (ComonadEnv ToolConfiguration w, Monad m, Listable b, FromJSON b) => Proxy b -> w a -> QueryString -> (RESTT m (Result [b]), w a)
queryList p w qs = maybe (errMissingToken, w)
                (\ t ->
                  let uri       = toURI $ listEndpoint p <?> B8.unpack (QS.toString qs)
                      resources = toList (listField p) <$> getJSONWith (authorisation t) uri
                  in (resources, w))
                (authToken (ask w))

emptyQuery :: QueryString
emptyQuery = QS.queryString []

errMissingToken :: (Monad m) => m (Result a)
errMissingToken = return $ error "no authentication token defined"

-- |Extract a typed result from a JSON output
fromResponse :: (FromJSON a) => Text -> Either String Value -> Result a
fromResponse key (Right (Object b)) = if H.member "message" b
  then error . show $ (b H.! "message")
  else case H.lookup key b of
    Just val -> either error Right $ A.parseEither parseJSON val
    _        -> error $ "cannot decode JSON value to a " ++ show key ++ ": " ++ show b

fromResponse key v =
  error $ "cannot decode JSON value to a " ++ show key ++ ": " ++ show v
