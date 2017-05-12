{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Network.DO(
  -- * Types
  Command,
  module Network.DO.Types,
  -- * Generic Commands
  listKeys, listSizes, listRegions, listImages,
  -- * Droplets Commands
  listDroplets, createDroplet, showDroplet, destroyDroplet,
  dropletAction, dropletConsole, getAction, listDropletSnapshots,
  -- * Floating IPs Commands
  listFloatingIPs, createFloatingIP, deleteFloatingIP, assignFloatingIP, unassignFloatingIP,
  -- * Domains Commands
  listDomains, createDomain, deleteDomain,
  listRecords, createRecord, deleteRecord,
  -- * Tags Commands
  listTags, createTag, retrieveTag, deleteTag,
  tagResources, untagResources,
  -- * Utilities
  runDOEnv, runDO, runDODebug, getAuthFromEnv, outputResult,
  generateName,
  module Network.DO.Droplets.Utils) where

import           Control.Exception            (catch, throw)
import           Control.Monad.Trans.Free
import           Data.IP
import           Network.DO.Droplets.Utils
import           Network.DO.Names
import           Network.DO.Net
import           Network.DO.Pairing
import           Network.DO.Pretty
import           Network.DO.Types             hiding(name, tagResources)
import           Network.REST
import           System.Environment           (getEnv)
import           System.IO.Error              (isDoesNotExistError)
import qualified Network.DO.Commands          as C
import qualified Network.DO.Domain            as C
import qualified Network.DO.Droplets.Commands as C
import qualified Network.DO.IP.Commands       as C
import qualified Network.DO.Tags.Commands     as C

type Command w a = FreeT (C.DO :+: C.DropletCommands :+: C.IPCommands :+: C.DomainCommands :+: C.TagsCommands) (RESTT w) a


listKeys :: (Monad w) => Command w [Key]
listKeys = injl C.listKeys

listSizes :: (Monad w) => Command w [Size]
listSizes = injl C.listSizes

listImages  :: (Monad w) => Command w  [Image]
listImages = injl C.listImages

listRegions :: (Monad w) => Command w [Region]
listRegions = injl C.listRegions

listFloatingIPs :: (Monad w) => Command w [FloatingIP]
listFloatingIPs = injrrl C.listFloatingIPs

createFloatingIP :: (Monad w) => FloatingIPTarget -> Command w (Result FloatingIP)
createFloatingIP = injrrl . C.createFloatingIP

deleteFloatingIP :: (Monad w) => IP -> Command w (Result ())
deleteFloatingIP = injrrl . C.deleteFloatingIP

assignFloatingIP :: (Monad w) => IP -> Id -> Command w (Result (ActionResult IPActionType))
assignFloatingIP ip did = injrrl $ C.floatingIPAction ip (AssignIP did)

unassignFloatingIP :: (Monad w) => IP -> Command w (Result (ActionResult IPActionType))
unassignFloatingIP ip = injrrl $ C.floatingIPAction ip UnassignIP

listDomains :: (Monad w) => Command w [Domain]
listDomains = injrrrl C.listDomains

createDomain :: (Monad w) => DomainName -> IP -> Command w (Result Domain)
createDomain dname ip = injrrrl $ C.createDomain dname ip

deleteDomain :: (Monad w) => DomainName -> Command w (Result ())
deleteDomain = injrrrl . C.deleteDomain

listRecords :: (Monad w) => DomainName -> Command w [DomainRecord]
listRecords = injrrrl . C.listRecords

createRecord :: (Monad w) => DomainName -> DomainRecord -> Command w (Result DomainRecord)
createRecord dname ip = injrrrl $ C.createRecord dname ip

deleteRecord :: (Monad w) => DomainName -> Id -> Command w (Result ())
deleteRecord dname rid = injrrrl $ C.deleteRecord dname rid

listDroplets :: (Monad w) => Command w [Droplet]
listDroplets = injrl C.listDroplets

createDroplet :: (Monad w) => BoxConfiguration -> Command w (Either Error Droplet)
createDroplet = injrl . C.createDroplet

showDroplet :: (Monad w) => Integer -> Command w (Either Error Droplet)
showDroplet = injrl . C.showDroplet

destroyDroplet :: (Monad w) => Integer -> Command w (Result ())
destroyDroplet = injrl . C.destroyDroplet

dropletAction :: (Monad w) => Id -> Action -> Command w (Result (ActionResult DropletActionType))
dropletAction did = injrl . C.dropletAction did

dropletConsole :: (Monad w) => Droplet -> Command w (Result ())
dropletConsole = injrl . C.dropletConsole

getAction :: (Monad w) => Id -> Id -> Command w (Result (ActionResult DropletActionType))
getAction  did = injrl . C.getAction did

listDropletSnapshots :: (Monad w) => Id -> Command w [Image]
listDropletSnapshots = injrl . C.listDropletSnapshots

listTags :: (Monad w) => Command w [Tag]
listTags = injrrrr C.listTags

createTag :: (Monad w) => TagName -> Command w (Either Error Tag)
createTag = injrrrr . C.createTag

retrieveTag :: (Monad w) => TagName -> Command w (Either Error Tag)
retrieveTag = injrrrr . C.retrieveTag

deleteTag :: (Monad w) => TagName -> Command w (Either Error ())
deleteTag = injrrrr . C.deleteTag

tagResources :: (Monad w) => TagName -> TagPairs -> Command w (Either Error ())
tagResources name = injrrrr . C.tagResources name

untagResources :: (Monad w) => TagName -> TagPairs -> Command w (Either Error ())
untagResources name = injrrrr . C.untagResources name



-- | Run DO actions, extracting authentication token from environment variable `AUTH_TOKEN`.
runDOEnv :: Command IO a -> IO a
runDOEnv actions = getAuthFromEnv >>= runDO actions

-- | Run DO actions, passing a built authentication token.
runDO :: Command IO a -> Maybe AuthToken -> IO a
runDO actions token =  runConduit NoDebug $ pairEffectM (\ _ b -> return b) (mkDOClient $ Tool Nothing token False) actions

-- | Run DO actions, debugging requests, passing a built authentication token.
runDODebug :: Command IO a -> Maybe AuthToken -> IO a
runDODebug actions token =  runConduit Debug $ pairEffectM (\ _ b -> return b) (mkDOClient $ Tool Nothing token False) actions


getAuthFromEnv :: IO (Maybe AuthToken)
getAuthFromEnv = (Just `fmap` getEnv "AUTH_TOKEN") `catch` (\ (e :: IOError) -> if isDoesNotExistError e then return Nothing else throw e)
