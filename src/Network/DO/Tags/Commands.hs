{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.DO.Tags.Commands(TagsCommands,
                                TagsCommandsT, CoTagsCommandsT,
                                CoTagsCommands(..),
                                TagName,
                                createTag, retrieveTag, listTags,
                                tagResources, untagResources,
                                deleteTag) where

import Prelude

import Control.Comonad.Trans.Cofree
import Control.Monad.Trans.Free
import Network.DO.Pairing
import Network.DO.Types hiding (TagResources, tagResources, name)

-- | Available commands for tags
data TagsCommands a = CreateTag TagName (Result Tag -> a)
                    | RetrieveTag TagName (Result Tag -> a)
                    | DeleteTag TagName (Result () -> a)
                    | ListTags ([Tag] -> a)
                    | TagResources TagName TagPairs (Result () -> a)
                    | UntagResources TagName TagPairs (Result () -> a)
                    deriving (Functor)

-- free transformer to embed effects
type TagsCommandsT = FreeT TagsCommands

-- smart constructors
createTag :: TagName -> TagsCommands (Result Tag)
createTag name = CreateTag name Prelude.id

retrieveTag :: TagName -> TagsCommands (Result Tag)
retrieveTag name = RetrieveTag name Prelude.id

deleteTag :: TagName -> TagsCommands (Result ())
deleteTag name = DeleteTag name Prelude.id

listTags :: TagsCommands [Tag]
listTags = ListTags Prelude.id

tagResources :: TagName -> TagPairs -> TagsCommands (Result ())
tagResources name pairs = TagResources name pairs Prelude.id

untagResources :: TagName -> TagPairs -> TagsCommands (Result ())
untagResources name pairs = TagResources name pairs Prelude.id

-- | Comonadic interpreter for @Tagscommands@
data CoTagsCommands m k = CoTagsCommands { createTagH      :: TagName -> (m (Result Tag), k)
                                         , retrieveTagH    :: TagName -> (m (Result Tag), k)
                                         , deleteTagH      :: TagName -> (m (Result ()), k)
                                         , listTagsH       :: (m [Tag], k)
                                         , tagResourcesH   :: TagName -> TagPairs -> (m (Result ()), k)
                                         , untagResourcesH :: TagName -> TagPairs -> (m (Result ()), k)
                                         } deriving Functor

-- | Cofree closure of CoTagsCommands functor
type CoTagsCommandsT m = CofreeT (CoTagsCommands m)

-- Pair DSL with interpreter within some monadic context
instance (Monad m) => PairingM (CoTagsCommands m) TagsCommands m where
  pairM f (CoTagsCommands create _ _ _ _ _)   (CreateTag name k)       = pairM f (create name) k
  pairM f (CoTagsCommands _ retrieve _ _ _ _) (RetrieveTag name k)     = pairM f (retrieve name) k
  pairM f (CoTagsCommands _ _ delete _ _ _)   (DeleteTag name k)       = pairM f (delete name) k
  pairM f (CoTagsCommands _ _ _ list _ _)     (ListTags k)             = pairM f list k
  pairM f (CoTagsCommands _ _ _ _ tag _)      (TagResources name pairs k)   = pairM f (tag name pairs) k
  pairM f (CoTagsCommands _ _ _ _ _ untag)    (UntagResources name pairs k) = pairM f (untag name pairs) k
