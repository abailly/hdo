{-# LANGUAGE ScopedTypeVariables #-}
-- | Implementation of @REST@ interface based on <https://haskell-lang.org/library/http-client http-conduit>
-- standard library
module Network.REST.Conduit where

import           Control.Concurrent       (threadDelay)
import           Control.Monad.Trans.Free
import           Data.Aeson               (Value, decode, eitherDecode)
import           Data.CaseInsensitive     (mk)
import           Data.Functor             (void)
import           Data.Text.Encoding       (encodeUtf8)
import           Network.HTTP.Simple
import           Network.REST.Commands
import           Network.URI

data Debug = NoDebug
           | Debug

debugRequest :: Debug -> Request -> IO ()
debugRequest NoDebug _   = pure ()
debugRequest Debug   req = putStrLn $ "request: " ++ show req

debugResponse :: (Show a) => Debug -> Response a -> IO ()
debugResponse NoDebug _   = pure ()
debugResponse Debug   req = putStrLn $ "response: " ++ show req

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
        debugRequest debug request
        response <- httpJSON request
        debugResponse debug response
        let value = getResponseBody response :: Value
        runConduit debug (k value)

      step (Free (GetWith opts uri k)) = do
        request <- parseRequest $ "GET " ++ asString uri
        debugRequest debug request
        response <- httpJSON $ options opts request
        debugResponse debug response
        let value = getResponseBody response :: Value
        runConduit debug (k value)

      step (Free (DeleteWith opts uri k)) = do
        request <- parseRequest $ "DELETE " ++ asString uri
        debugRequest debug request
        void $ httpLBS $ options opts request
        runConduit debug k

      step (Free (Post uri val k)) = do
        request <- parseRequest $ "POST " ++ asString uri
        debugRequest debug request
        response <- httpLbs $ setRequestBodyJSON val request
        debugResponse debug response
        let value = decode $ getResponseBody response
        runConduit debug (k value)

      step (Free (PostWith opts uri val k)) = do
        request <- parseRequest $ "POST " ++ asString uri
        debugRequest debug request
        response <- httpLbs $ options opts $ setRequestBodyJSON val request
        debugResponse debug response
        let value :: Either String Value = eitherDecode $ getResponseBody response
        runConduit debug (k value)


options :: Options -> Request -> Request
options (Header h c) = setRequestHeader (mk $ encodeUtf8 h) (map encodeUtf8 c)
