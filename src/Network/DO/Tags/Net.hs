{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Network interpreter for Tags specific API
module Network.DO.Tags.Net(tagsCommandsInterpreter) where

import Prelude                      as P hiding (error)

import Control.Comonad.Env.Class    (ComonadEnv, ask)
import Control.Monad.Trans          (MonadIO)
import Data.Aeson                   as A hiding (Result, pairs)
import Data.Proxy
import Network.DO.Tags.Commands
import Network.DO.Net.Common
import Network.DO.Types             as DO hiding (URI, name)
import Network.REST


-- Define some shortcut accessor
tagsURI :: String
tagsURI = "tags"

tagsEndpoint :: String
tagsEndpoint = rootURI </> apiVersion </> tagsURI


-- Derive the listTags endpoint for free, see @Network.DO.Net.Common@
--
-- https://developers.digitalocean.com/documentation/v2/#list-all-tags
--
instance Listable Tag where
  listEndpoint _ = tagsEndpoint
  listField    _ = "tags"

--
-- https://developers.digitalocean.com/documentation/v2/#create-a-new-tag
--
doCreateTag :: (ComonadEnv ToolConfiguration w, Monad m) => w a -> TagName -> (RESTT m (Result Tag), w a)
doCreateTag w name = maybe (errMissingToken, w) runQuery (authToken (ask w))
  where
    runQuery t = let opts  = authorisation t
                     body  = toJSON . object $ ["name" .= name]
                     query = fromResponse "tag" <$> postJSONWith opts (toURI tagsEndpoint) body
                 in (query, w)

--
-- https://developers.digitalocean.com/documentation/v2/#retrieve-a-tag
--
doRetrieveTag :: (ComonadEnv ToolConfiguration w, Monad m) => w a -> TagName -> (RESTT m (Result Tag), w a)
doRetrieveTag w name = maybe (errMissingToken, w) runQuery (authToken (ask w))
  where
    runQuery t = let opts  = authorisation t
                     query = fromResponse "tag" . Right <$> getJSONWith opts (toURI $ tagsEndpoint </> name)
                 in (query, w)

--
-- https://developers.digitalocean.com/documentation/v2/#delete-a-tag
--
doDeleteTag :: (ComonadEnv ToolConfiguration w, Monad m) => w a -> TagName -> (RESTT m (Result ()), w a)
doDeleteTag w name = maybe (errMissingToken, w) runQuery (authToken (ask w))
  where
    runQuery t = let opts  = authorisation t
                     body  = toJSON ()
                     query = Right () <$ deleteJSONWith opts (toURI $ tagsEndpoint </> name) body
                 in (query, w)

--
-- https://developers.digitalocean.com/documentation/v2/#tag-a-resource
--
doTagResources :: (ComonadEnv ToolConfiguration w, Monad m) => w a -> TagName -> TagPairs-> (RESTT m (Result ()), w a)
doTagResources w name pairs = maybe (errMissingToken, w) runQuery (authToken (ask w))
  where
    runQuery t = let opts  = authorisation t
                     body  = toJSON pairs
                     query = Right () <$ postJSONWith opts (toURI $ tagsEndpoint </> name </> "resources") body
                 in (query, w)

--
-- https://developers.digitalocean.com/documentation/v2/#untag-a-resource
--
doUntagResources :: (ComonadEnv ToolConfiguration w, Monad m) => w a -> TagName -> TagPairs -> (RESTT m (Result ()), w a)
doUntagResources w name pairs = maybe (errMissingToken, w) runQuery (authToken (ask w))
  where
    runQuery t = let opts  = authorisation t
                     body  = toJSON pairs
                     query = Right () <$ deleteJSONWith opts (toURI $ tagsEndpoint </> name </> "resources") body
                 in (query, w)


-- | DSL Interpreter for TagsCommands into IO via the REST DSL
tagsCommandsInterpreter :: (MonadIO m, ComonadEnv ToolConfiguration w) => w a -> CoTagsCommands (RESTT m) (w a)
tagsCommandsInterpreter = CoTagsCommands
                          <$> doCreateTag
                          <*> doRetrieveTag
                          <*> doDeleteTag
                          <*> queryList (Proxy :: Proxy Tag)
                          <*> doTagResources
                          <*> doUntagResources
