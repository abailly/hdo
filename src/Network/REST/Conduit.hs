{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Implementation of @REST@ interface based on <https://haskell-lang.org/library/http-client http-conduit>
-- standard library
module Network.REST.Conduit where

import           Control.Concurrent         (threadDelay)
import           Control.Monad.Trans.Free
import           Data.Aeson                 (ToJSON, Value, decode,
                                             eitherDecode, encode)
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.CaseInsensitive       (mk)
import           Data.Functor               (void)
import           Data.Text.Encoding         (encodeUtf8)
import           Network.REST.Commands
import           Network.URI

data Debug where
  NoDebug   :: Debug
  Debug     :: Debug

data RequestBody where
  Body :: forall a . (ToJSON a) => a -> RequestBody

data RequestLog where
  RequestLog         :: Request                -> RequestLog
  RequestLogWithBody :: Request -> RequestBody -> RequestLog

debugRequest :: Debug -> RequestLog -> IO ()
debugRequest NoDebug _                                 = pure ()
debugRequest Debug   (RequestLog req)                  = putStrLn $ "request: " ++ show req
debugRequest Debug   (RequestLogWithBody req (Body b)) = putStrLn $ "request: " ++ show req ++ ", body: " ++ unpack (encode b)

debugResponse :: (Show a) => Debug -> IO (Response a) -> IO (Response a)
debugResponse NoDebug act = act
debugResponse Debug   act = do
  response <- act
  putStrLn $ "response: " ++ show response
  pure response

-- | An implementation of @REST@ functor based on http-client and @IO@
runConduit :: Debug -> RESTT IO r -> IO r
runConduit debug r = do
  mr <- runFreeT r
  step mr
    where
      asString :: URI -> String
      asString = ($ "") . uriToString id

      step (Pure value)       = return value

      step (Free (WaitFor delay message k))  = do
        putStrLn message
        threadDelay delay
        runConduit debug k

      step (Free (Get uri k)) = do
        request <- parseRequest $ "GET " ++ asString uri
        debugRequest debug (RequestLog request)
        response <- debugResponse debug $ httpJSON request
        let value = getResponseBody response :: Value
        runConduit debug (k $ Right value)

      step (Free (GetWith opts uri k)) = do
        request <- parseRequest $ "GET " ++ asString uri
        debugRequest debug (RequestLog request)
        response <- debugResponse debug $ httpJSON $ options opts request
        let value = getResponseBody response :: Value
        runConduit debug (k value)

      step (Free (DeleteWith opts uri k)) = do
        request <- parseRequest $ "DELETE " ++ asString uri
        debugRequest debug (RequestLog request)
        void $ httpLBS $ options opts request
        runConduit debug k

      step (Free (Post uri val k)) = do
        request <- parseRequest $ "POST " ++ asString uri
        debugRequest debug (RequestLogWithBody request (Body val))
        response <- debugResponse debug  $ httpLbs $ setRequestBodyJSON val request
        let value = decode $ getResponseBody response
        runConduit debug (k value)

      step (Free (PostWith opts uri val k)) = do
        request <- parseRequest $ "POST " ++ asString uri
        debugRequest debug (RequestLogWithBody request (Body val))
        response <- httpLbs $ options opts $ setRequestBodyJSON val request
        debugResponse debug response
        let value :: Either String Value = eitherDecode $ getResponseBody response
        runConduit debug (k value)


options :: Options -> Request -> Request
options (Header h c) = setRequestHeader (mk $ encodeUtf8 h) (map encodeUtf8 c)
