{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Network interpreter for Floating Domains specific API
module Network.DO.Domain.Net(dnsCommandsInterpreter) where

import           Control.Applicative
import           Control.Comonad.Env.Class  (ComonadEnv, ask)
import           Control.Monad.Trans        (MonadIO)
import           Data.Aeson                  as A hiding (Result)
import           Data.ByteString.Char8       as B8
import           Data.IP
import           Data.Maybe
import           Data.Proxy
import           Network.DO.Domain.Commands
import           Network.DO.Net.Common
import           Network.DO.Types            as DO hiding (URI)
import           Network.HTTP.QueryString   (QueryString)
import           Network.REST
import           Prelude                     as P hiding (error)
import qualified Network.HTTP.QueryString    as QS

domainsURI :: String
domainsURI = "domains"

domainsEndpoint :: String
domainsEndpoint = rootURI </> apiVersion </> domainsURI

instance Listable Domain where
  listEndpoint _ = domainsEndpoint
  listField _    = "domains"

doCreateDomain :: (ComonadEnv ToolConfiguration w, Monad m) => w a -> DomainName -> IP -> (RESTT m (Result Domain), w a)
doCreateDomain w name ip = maybe (errMissingToken, w)
  runQuery
  (authToken (ask w))
  where
    runQuery t = let opts   = authorisation t
                     domain = postJSONWith opts (toURI domainsEndpoint) (toJSON config) >>= return . fromResponse "domain"
                 in (domain, w)

      where
        config = DomainConfig name ip

doDeleteDomain :: (ComonadEnv ToolConfiguration w, Monad m) => w a -> DomainName -> (RESTT m (Result ()), w a)
doDeleteDomain w name = maybe (errMissingToken, w)
                  (\ t -> let r = deleteJSONWith (authorisation t) (toURI $ domainsEndpoint </> show name) (toJSON ()) >> return (Right ())
                          in (r, w))
                  (authToken (ask w))

doListRecords :: (ComonadEnv ToolConfiguration w, Monad m) => w a -> QueryString -> DomainName -> (RESTT m (Result [DomainRecord]), w a)
doListRecords w qs name = maybe (errMissingToken, w)
                       (\ t ->
                         let uri     = toURI $ domainsEndpoint </> show name </> "records" <?> B8.unpack (QS.toString qs)
                             records = toList "domain_records" <$> getJSONWith (authorisation t) uri
                         in (records, w))
                       (authToken (ask w))

doCreateRecord :: (ComonadEnv ToolConfiguration w, Monad m) => w a -> DomainName -> DomainRecord -> (RESTT m (Result DomainRecord), w a)
doCreateRecord w name record =
  maybe (errMissingToken,  w)
  runQuery
  (authToken (ask w))
  where
    runQuery t = let opts   = authorisation t
                     domain = postJSONWith opts (toURI $ domainsEndpoint </> show name </> "records") (toJSON record) >>= return . fromResponse "domain_record"
                 in (domain, w)

doDeleteRecord :: (ComonadEnv ToolConfiguration w, Monad m) => w a -> DomainName -> Id -> (RESTT m (Result ()), w a)
doDeleteRecord w name rid =
  maybe (errMissingToken, w)
  (\ t -> let r = deleteJSONWith (authorisation t) (toURI $ domainsEndpoint </> show name </> "records" </> show rid ) (toJSON ()) >> return (Right ())
          in (r, w))
  (authToken (ask w))

dnsCommandsInterpreter :: (MonadIO m, ComonadEnv ToolConfiguration w) => w a -> CoDomainCommands (RESTT m) (w a)
dnsCommandsInterpreter = CoDomainCommands
                        <$> queryList (Proxy :: Proxy Domain)
                        <*> doCreateDomain
                        <*> doDeleteDomain
                        <*> doListRecords
                        <*> doCreateRecord
                        <*> doDeleteRecord

