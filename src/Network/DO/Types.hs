{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |Contains Haskell representation of most data types used for interacting with
-- DigitalOcean's API v2
--
-- See https://developers.digitalocean.com/documentation/v2/
module Network.DO.Types where

import           Data.Aeson         as A hiding (Error, Result)
import           Data.Aeson.Types   as A hiding (Error, Result)
import           Data.Default
import           Data.Maybe        (isNothing)
import qualified Data.HashMap.Lazy  as H
import           Data.IP
import           Data.List         (elemIndex, concat)
import           Data.Monoid       ((<>))
import           Data.Text         (pack, unpack)
import           Data.Time         (UTCTime)
import           GHC.Generics
import qualified Text.Parsec        as P

type AuthToken = String

type Slug = String

type URI = String

newtype Error = Error { msg :: String } deriving (Eq, Show, Read)

type Result a = Either Error a

error :: String -> Result a
error = Left . Error

data ToolConfiguration = Tool { slackUri  :: Maybe URI
                              , authToken :: Maybe AuthToken
                              , quiet     :: Bool
                              } deriving (Show,Read)

instance Default ToolConfiguration where
  def = Tool Nothing Nothing False

-- | A type for describing @Region@
-- A region can be assigned an empty object when it is undefined, or be referenced simply
-- by its @slug@
-- https://developers.digitalocean.com/documentation/v2/#regions
data Region = Region { regionName      :: String
                     , regionSlug      :: Slug
                     , regionSizes     :: [ SizeSlug ]
                     , regionAvailable :: Bool
                     }
            | RegionSlug Slug
            | NoRegion

instance ToJSON Region where
  toJSON (RegionSlug s) = toJSON s
  toJSON  NoRegion      = object []
  toJSON Region{..}     = object [ "name" .= regionName
                                 , "slug" .= regionSlug
                                 , "sizes" .= regionSizes
                                 , "available" .= regionAvailable
                                 ]

instance Show Region where
  show (RegionSlug s) = s
  show  NoRegion      = "NoRegion"
  show Region{..}     = "Region { regionName = " <> show regionName <>
                        ", regionSlug = " <> show regionSlug <>
                        ", regionSizes = " <> show regionSizes <>
                        ", regionAvailable = " <> show regionAvailable <>
                        "}"

instance FromJSON Region where
  parseJSON (String s) = return $ RegionSlug (unpack s)
  parseJSON (Object o) = if H.null o
                         then return NoRegion
                         else Region
                              <$> o .: "name"
                              <*> o .: "slug"
                              <*> o .: "sizes"
                              <*> o .: "available"
  parseJSON e          = failParse e

-- | String representation of size slugs
-- This maps to corresponding expected JSON string value.
sizeSlugs :: [String]
sizeSlugs = [ "512mb", "1gb",  "2gb",  "4gb",  "8gb",  "16gb", "32gb", "48gb", "64gb", "96gb"  ]

-- | Enumeration of all possible size slugs
data SizeSlug = M512 | G1 | G2 | G4 | G8 | G16 | G32 | G48 | G64 | G96
              deriving (Enum,Ord,Eq)

instance Show SizeSlug where
  show sz = sizeSlugs !! fromEnum sz

instance Read SizeSlug where
  readsPrec _ sz = case elemIndex sz sizeSlugs of
                    Just i  -> return (toEnum i, "")
                    Nothing -> fail $ "cannot parse " <> sz

instance ToJSON SizeSlug where
  toJSON sz = toJSON $ sizeSlugs !! fromEnum sz

instance FromJSON SizeSlug where
  parseJSON (String s) = case elemIndex (unpack s) sizeSlugs of
                          Just i -> return $ toEnum i
                          Nothing -> fail $ "cannot parse " <> unpack s
  parseJSON e          = failParse e


type ImageSlug = String
type KeyId = Int

defaultImage :: ImageSlug
defaultImage = "ubuntu-14-04-x64"

data BoxConfiguration = BoxConfiguration { configName       :: String
                                         , boxRegion        :: Region
                                         , size             :: SizeSlug
                                         , configImageSlug  :: ImageSlug
                                         , keys             :: [KeyId]
                                         , backgroundCreate :: Bool
                                         } deriving (Show)

instance ToJSON BoxConfiguration where
  toJSON BoxConfiguration{..} = object [ "name"     .=  configName
                                       , "region"   .=  boxRegion
                                       , "size"     .=  size
                                       , "image"    .=  configImageSlug
                                       , "ssh_keys" .=  keys
                                       , "backups"  .=  False
                                       , "ipv6"     .=  False
                                       , "private_networking" .=  False
                                       ]

type Id = Integer

data Mega
data Giga

-- | A type for various sizes
-- Type parameter is used to define number's magnitude
newtype Bytes a = Bytes { bytesSize :: Int } deriving Show

jsonBytes :: Int -> Parser (Bytes a)
jsonBytes = return . Bytes

instance FromJSON (Bytes Mega) where
  parseJSON (Number n) = jsonBytes (truncate n)
  parseJSON e          = failParse e

instance FromJSON (Bytes Giga) where
  parseJSON (Number n) = jsonBytes (truncate n)
  parseJSON e          = failParse e

instance ToJSON (Bytes a) where
  toJSON Bytes{..} = toJSON bytesSize


newtype Date = Date { theDate :: UTCTime } deriving Show

instance FromJSON Date where
  parseJSON d@(String _) = Date <$> parseJSON d
  parseJSON e            = failParse e

instance ToJSON Date where
  toJSON Date{..} = toJSON theDate

data Status = New
            | Active
            | Off
            | Archive
            deriving (Eq,Show)

instance FromJSON Status where
  parseJSON (String s) = case s of
                          "new"      -> return New
                          "active"   -> return Active
                          "off"      -> return Off
                          "archive"  -> return Archive
                          _          -> fail $ "cannot parse " <> unpack s
  parseJSON e          = failParse e

instance ToJSON Status where
  toJSON New     = "new"
  toJSON Active  = "active"
  toJSON Off     = "off"
  toJSON Archive = "archive"


data NetType = Public | Private deriving (Show, Eq)

-- | Type of a single Network definition
--
-- This type is parameterized with a phantom type which lifts the network address type at
-- the type level (could use DataKinds extension...). This allows distinguishing types of
-- of networks while using same parsing.
data Network a = NetworkV4 { ip_address :: IP
                           , netmask    :: IP
                           , gateway    :: IP
                           , netType    :: NetType
                           }
               | NetworkV6 { ip_address :: IP
                           , netmask_v6 :: Int
                           , gateway    :: IP
                           , netType    :: NetType
                           } deriving Show

instance FromJSON IP where
  parseJSON (String s) = return $ read $ unpack s
  parseJSON e          = fail $ "cannot parse IP " <> show e

instance ToJSON IP where
  toJSON = String . pack . show

instance FromJSON NetType where
  parseJSON (String s) = case s of
                          "public" -> return Public
                          "private" -> return Private
                          e         -> failParse e
  parseJSON e          = failParse e

instance ToJSON NetType where
  toJSON Public  = "public"
  toJSON Private = "private"

data V4
data V6

jsonNetwork :: (FromJSON a3, FromJSON a2, FromJSON a1, FromJSON a) => (a3 -> a2 -> a1 -> a -> b) -> Object -> Parser b
jsonNetwork f n = f
                  <$> (n .: "ip_address")
                  <*> (n .: "netmask")
                  <*> (n .: "gateway")
                  <*> (n .: "type")

toJsonNetwork :: (ToJSON netmask) => IP -> netmask -> IP -> NetType -> Value
toJsonNetwork ip_address netmask gateway netType = object [ "ip_address" .= ip_address
                                                          , "netmask"    .= netmask
                                                          , "gateway"    .= gateway
                                                          , "type"       .= netType
                                                          ]

instance FromJSON (Network V4) where
  parseJSON (Object n) = jsonNetwork NetworkV4 n
  parseJSON e          = failParse e

instance ToJSON (Network V4) where
  toJSON NetworkV4{..} = toJsonNetwork ip_address netmask gateway netType
  toJSON NetworkV6{..} = toJsonNetwork ip_address netmask_v6 gateway netType

instance FromJSON (Network V6) where
  parseJSON (Object n) = jsonNetwork NetworkV6 n
  parseJSON e          = fail $ "cannot parse network v6 " <> show e

instance ToJSON (Network V6) where
  toJSON NetworkV4{..} = toJsonNetwork ip_address netmask gateway netType
  toJSON NetworkV6{..} = toJsonNetwork ip_address netmask_v6 gateway netType


-- | Type of Networks configured for a @Droplet@
--
-- A network is either a list of IPv4 and IPv6 NICs definitions, or no network. We need this
-- because a droplet can contain an ''empty'' @networks@  JSON Object entry, instead of @null@.
data Networks = Networks { v4 :: [ Network V4 ]
                         , v6 :: [ Network V6 ]
                         }
              | NoNetworks
              deriving (Generic, Show)

instance FromJSON Networks where
  parseJSON (Object n) = if H.null n
                         then return NoNetworks
                         else Networks
                              <$> (n .: "v4")
                              <*> (n .: "v6")
  parseJSON e          = fail $ "cannot parse network v6 " <> show e

instance ToJSON Networks where
  toJSON NoNetworks = Null
  toJSON Networks{..} = object [ "v4" .= v4
                               , "v6" .= v6
                               ]

-- | (Partial) Type of Droplets
--
-- https://developers.digitalocean.com/documentation/v2/#droplets
data Droplet = Droplet { dropletId    :: Id
                       , name         :: String
                       , memory       :: Bytes Mega
                       , vcpus        :: Int
                       , disk         :: Bytes Giga
                       , locked       :: Bool
                       , created_at   :: Date
                       , status       :: Status
                       , backup_ids   :: [ Id ]
                       , snapshot_ids :: [ Id ]
                       , region       :: Region
                       , size_slug    :: SizeSlug
                       , networks     :: Networks
                       } deriving (Show)

instance FromJSON Droplet where
  parseJSON (Object o) = Droplet
                         <$> o .: "id"
                         <*> o .: "name"
                         <*> o .: "memory"
                         <*> o .: "vcpus"
                         <*> o .: "disk"
                         <*> o .: "locked"
                         <*> o .: "created_at"
                         <*> o .: "status"
                         <*> o .: "backup_ids"
                         <*> o .: "snapshot_ids"
                         <*> o .: "region"
                         <*> o .: "size_slug"
                         <*> o .: "networks"
  parseJSON e          = fail $ "cannot parse network v6 " <> show e

instance ToJSON Droplet where
  toJSON Droplet{..} = object [ "id"           .= dropletId
                              , "name"         .= name
                              , "memory"       .= memory
                              , "vcpus"        .= vcpus
                              , "disk"         .= disk
                              , "locked"       .= locked
                              , "created_at"   .= created_at
                              , "status"       .= status
                              , "backup_ids"   .= backup_ids
                              , "snapshot_ids" .= snapshot_ids
                              , "region"       .= region
                              , "size_slug"    .= size_slug
                              , "networks"     .= networks
                              ]


data ImageType = Snapshot
               | Temporary
               | Backup
                 deriving Show

instance FromJSON ImageType where
  parseJSON (String s) = case s of
                          "snapshot" -> return Snapshot
                          "temporary" -> return Temporary
                          "backup" -> return Backup
                          _        -> fail $ "cannot parse " <> unpack s
  parseJSON e          = failParse e

-- | Type of droplet images
--
-- https://developers.digitalocean.com/documentation/v2/#images
data Image = Image { imageId          :: Id
                   , imageName        :: String
                   , distribution     :: String
                   , imageSlug        :: Maybe Slug
                   , publicImage      :: Bool
                   , imageRegions     :: [ Region ]
                   , min_disk_size    :: Bytes Giga
                   , image_created_at :: Date
                   , imageType        :: ImageType
                   } deriving Show

instance FromJSON Image where
  parseJSON (Object o) = Image
                         <$> o .: "id"
                         <*> o .: "name"
                         <*> o .: "distribution"
                         <*> o .:? "slug"
                         <*> o .: "public"
                         <*> o .: "regions"
                         <*> o .: "min_disk_size"
                         <*> o .: "created_at"
                         <*> o .: "type"
  parseJSON e          = failParse e

-- | Type of SSH @Key@s
--
--https://developers.digitalocean.com/documentation/v2/#ssh-keys
data Key = Key { keyId          :: Id
               , keyFingerprint :: String
               , publicKey      :: String
               , keyName        :: String
               } deriving Show

instance FromJSON Key where
  parseJSON (Object o) = Key
                         <$> o .: "id"
                         <*> o .: "fingerprint"
                         <*> o .: "public_key"
                         <*> o .: "name"
  parseJSON e          = failParse e

type TransferRate = Double
type Price = Double

-- | Type of Size objects
--
-- https://developers.digitalocean.com/documentation/v2/#sizes
data Size = Size { szSlug          :: SizeSlug
                 , szMemory        :: Bytes Mega
                 , szVcpus         :: Int
                 , szDisk          :: Bytes Giga
                 , szTransfer      :: TransferRate
                 , szPrice_Monthly :: Price
                 , szPrice_Hourly  :: Price
                 , szRegions       :: [ Region ]
                 , szAvailable     :: Bool
                 } deriving (Show)


instance FromJSON Size where
  parseJSON (Object o) = Size
                         <$> o .: "slug"
                         <*> o .: "memory"
                         <*> o .: "vcpus"
                         <*> o .: "disk"
                         <*> o .: "transfer"
                         <*> o .: "price_monthly"
                         <*> o .: "price_hourly"
                         <*> o .: "regions"
                         <*> o .: "available"

  parseJSON e          = failParse e

-- * Droplets Actions

-- | Type of action status
-- This is returned when action is initiated or when status of some action is requested

data ActionResult result = ActionResult { actionId           :: Id
                                        , actionStatus       :: ActionStatus
                                        , actionType         :: result
                                        , actionStartedAt    :: Maybe Date
                                        , actionCompletedAt  :: Maybe Date
                                        , actionResourceId   :: Id
                                        , actionResourceType :: String
                                        , actionRegionSlug   :: Region
                                        } deriving (Show)

instance (FromJSON r) => FromJSON (ActionResult r) where
  parseJSON (Object o) = ActionResult
                         <$> o .: "id"
                         <*> o .: "status"
                         <*> o .: "type"
                         <*> o .:? "started_at"
                         <*> o .:? "completed_at"
                         <*> o .: "resource_id"
                         <*> o .: "resource_type"
                         <*> o .: "region_slug"
  parseJSON v          = fail $ "cannot parse action " ++ show v

data ActionStatus = InProgress
                  | Completed
                  | Errored
                  deriving (Show)

instance FromJSON ActionStatus where
  parseJSON (String s) = case s of
                          "in-progress" -> return InProgress
                          "completed"   -> return Completed
                          "errored"     -> return Errored
                          _             -> fail $ "unknown action status " ++ show s
  parseJSON v          = fail $ "cannot parse action status " ++ show v

data DropletActionType = PowerOff
                | PowerOn
                | MakeSnapshot
                deriving (Show)

instance FromJSON DropletActionType where
  parseJSON (String s) = case s of
                          "power_off" -> return PowerOff
                          "power_on"  -> return PowerOn
                          "snapshot"  -> return MakeSnapshot
                          _           -> fail $ "unknown action type " ++ show s
  parseJSON v          = fail $ "cannot parse action type " ++ show v

instance ToJSON DropletActionType where
  toJSON PowerOff = String "power_off"
  toJSON PowerOn  = String "power_on"
  toJSON MakeSnapshot = String "snapshot"

data Action = DoPowerOff
            | DoPowerOn
            | CreateSnapshot String
            deriving Show

instance ToJSON Action where
  toJSON DoPowerOff                    = object [ "type" .= PowerOff ]
  toJSON DoPowerOn                     = object [ "type" .= PowerOn ]
  toJSON (CreateSnapshot snapshotName) = object [ "type" .= MakeSnapshot
                                                , "name" .= snapshotName
                                                ]

-- |Type of Domain zones
--
-- https://developers.digitalocean.com/documentation/v2/#domains

newtype DomainName = DomainName { domain :: String }

instance Show DomainName where
  show = domain

instance Read DomainName where
  readsPrec _ s = [(DomainName s,[])]

instance FromJSON DomainName where
  parseJSON (String s) = pure $ DomainName $ unpack s
  parseJSON e          = failParse e

instance ToJSON DomainName where
  toJSON (DomainName n) = String (pack n)

data Domain = Domain { domainName :: DomainName
                     , domainTTL  :: Maybe Int
                     , zone_file  :: Maybe String
                     } deriving (Show)

instance FromJSON Domain where
  parseJSON (Object o) = Domain
                         <$> o .: "name"
                         <*> o .: "ttl"
                         <*> o .: "zone_file"

  parseJSON e          = failParse e

data DomainConfig = DomainConfig DomainName IP

instance ToJSON DomainConfig where
  toJSON (DomainConfig name ip) = object [ "name" .= name
                                         , "ip_address" .= ip
                                         ]

-- | Enumeration of possible DNS records types
data DNSType = A | CNAME | TXT | PTR | SRV | NS | AAAA | MX
             deriving (Show, Read, Generic)

instance FromJSON DNSType
instance ToJSON DNSType

-- | Type of Domain zone file entries
--
-- https://developers.digitalocean.com/documentation/v2/#domain-records
data DomainRecord = DomainRecord { recordId       :: Id
                                 , recordType     :: DNSType
                                 , recordName     :: String
                                 , recordData     :: String
                                 , recordPriority :: Maybe Int
                                 , recordPort     :: Maybe Int
                                 , recordWeight   :: Maybe Int
                                 } deriving (Show)


instance FromJSON DomainRecord where
  parseJSON (Object o) = DomainRecord
                         <$> o .: "id"
                         <*> o .: "type"
                         <*> o .: "name"
                         <*> o .: "data"
                         <*> o .: "priority"
                         <*> o .: "port"
                         <*> o .: "weight"

  parseJSON e          = failParse e

instance ToJSON DomainRecord where
  toJSON DomainRecord{..} = object [ "type" .= recordType
                                   , "name" .= recordName
                                   , "data" .= recordData
                                   , "priority" .= recordPriority
                                   , "port" .= recordPort
                                   , "weight" .= recordWeight
                                   ]

parseRecord :: String -> Result DomainRecord
parseRecord s =
  case P.parse recordParser "" s of
    Left e  -> Left (Error $ show e)
    Right r -> Right r
  where
    recordParser :: P.Parsec String s DomainRecord
    recordParser = do
      t <- typeParser
      P.spaces
      n <- nameParser
      P.spaces
      d <- dataParser
      (prio, port, wei) <- recordAttributes t
      return $ DomainRecord 0 t n d prio port wei

    typeParser :: P.Parsec String s DNSType
    typeParser = P.choice [ rtype A , rtype CNAME , rtype TXT , rtype PTR , rtype SRV , rtype NS , rtype AAAA , rtype MX ]

    rtype :: DNSType -> P.Parsec String s DNSType
    rtype t = P.string (show t) >> return t

    nameParser :: P.Parsec String s String
    nameParser = P.many1 (P.lower P.<|> P.char '.')

    dataParser :: P.Parsec String s String
    dataParser = P.many1 (P.alphaNum P.<|> P.oneOf [ '.' ])

    recordAttributes :: DNSType -> P.Parsec String s (Maybe Int, Maybe Int, Maybe Int)
    recordAttributes SRV =  (,,) <$>
                            (Just <$> number) <*>
                            (Just <$> number) <*>
                            (Just <$> number)
    recordAttributes MX  =  (,,) <$>
                            (Just <$> number) <*>
                            pure Nothing <*>
                            pure Nothing
    recordAttributes _   = (,,) <$>
                           pure Nothing <*>
                           pure Nothing <*>
                           pure Nothing
    number :: P.Parsec String s Int
    number = P.spaces >> read <$> P.many1 P.digit

-- | Floating IPs
-- https://developers.digitalocean.com/documentation/v2/#floating-ips

data FloatingIP = FloatingIP { floatingIp      :: IP
                             , floatingDroplet :: Maybe Droplet
                             , floatingRegion  :: Region
                             } deriving (Show)

instance FromJSON FloatingIP where
  parseJSON (Object o) = FloatingIP
                         <$> o .: "ip"
                         <*> o .:? "droplet"
                         <*> o .: "region"

  parseJSON e          = failParse e

data FloatingIPTarget = TargetRegion Slug
                      | TargetDroplet Id
                        deriving (Show)

instance ToJSON FloatingIPTarget where
  toJSON (TargetRegion r)  = object [ "region" .= r ]
  toJSON (TargetDroplet i) = object [ "droplet_id" .= i ]

data IPAction = AssignIP Id
              | UnassignIP
  deriving (Show, Read)

instance ToJSON IPAction where
  toJSON (AssignIP did) = object [ "type" .= ("assign" :: String)
                                 , "droplet_id" .= did
                                 ]
  toJSON UnassignIP     = object [ "type" .= ("unassign" :: String)]

data IPActionType = Assign
                  | Unassign
                deriving (Show)

instance FromJSON IPActionType where
  parseJSON (String s) = case s of
                          "assign_ip" -> return Assign
                          "unassign_ip"  -> return Unassign
                          _           -> fail $ "unknown action type " ++ show s
  parseJSON v          = failParse v


-- | Type of Resources
data ResourceType
  = ResourceDroplet
  | ResourceVolume
  | ResourceBackend
  deriving (Eq, Ord, Enum)

resourceTypes :: [String]
resourceTypes = ["droplet", "volume", "backend"]

instance Show ResourceType where
  show r = resourceTypes !! fromEnum r

instance Read ResourceType where
  readsPrec _ sz = case elemIndex sz resourceTypes of
                    Just i  -> return (toEnum i, "")
                    Nothing -> fail $ "cannot parse " <> sz

instance FromJSON ResourceType where
  parseJSON (String v) =
    case elemIndex (unpack v) resourceTypes of
      Just i ->
        return $ toEnum i
      Nothing ->
        failParse ("cannot parse " <> v)

  parseJSON e = failParse e

instance ToJSON ResourceType where
  toJSON = toJSON . show

-- | Type of Block Storage (Volume)
--
-- https://developers.digitalocean.com/documentation/v2/#block-storage
data Volume = Volume
  { volumeId            :: Id      -- ^ The unique identifier for the Block Storage Volume.
  , volumeRegion        :: Region  -- ^ The region that the Block Storage Volume is located in.
  , volumeDropletIds    :: [Id]    -- ^ An array containing the IDs of the Droplets the volume is attached to.
  , volumeName          :: String  -- ^ A human-redable name for the Block Storage Volume.
  , volumeDescription   :: String  -- ^ An optional free-form text field to describe a Block Storage Volume.
  , volumeSizeGigaBytes :: Int     -- ^ The size of the Block Storage Volume in GiB (1024^3)
  , volumeCreatedAt     :: Date    -- ^ A time value that represents when the Block Storage Volume was created.
  } deriving (Show)

instance FromJSON Volume where
  parseJSON (Object o) = Volume
                         <$> o .: "id"
                         <*> o .: "region"
                         <*> o .: "droplet_ids"
                         <*> o .: "name"
                         <*> o .: "description"
                         <*> o .: "size_gigabytes"
                         <*> o .: "created_at"

  parseJSON e          = failParse e

instance ToJSON Volume where
  toJSON Volume{..} = object [ "id"             .= volumeId
                             , "region"         .= volumeRegion
                             , "droplet_ids"    .= volumeDropletIds
                             , "name"           .= volumeName
                             , "description"    .= volumeDescription
                             , "size_gigabytes" .= volumeSizeGigaBytes
                             , "created_at"     .= volumeSizeGigaBytes
                             ]


-- | Type of Tags
--
-- https://developers.digitalocean.com/documentation/v2/#tags

type TagName = String

data Tag = Tag
  { tagName       :: TagName       -- ^ The tag name
  , tagResources_ :: TagResources  -- ^ An embedded object containing key value pairs of resource type and resource statistics
  } deriving (Show)

data TagResources = TagResources
  { tagDroplets :: TagDroplets  -- ^ Statistics about the droplets resources
  , tagVolumes  :: TagVolumes   -- ^ Statistics about the volumes resources
  -- NOTE backend resources seem to exist too but I can't find any representation of them :|
  } deriving (Show)

data TagDroplets = TagDroplets
  { tagDropletsCount      :: Int            -- ^ The number of tagged droplets
  , tagDropletsLastTagged :: Maybe Droplet  -- ^ The last tagged droplet
  } deriving (Show)

data TagVolumes = TagVolumes
  { tagVolumesCount      :: Int           -- ^ The number of tagged volumes
  , tagVolumesLastTagged :: Maybe Volume  -- ^ The last tagged volume
  } deriving (Show)

data TagPairs = TagPairs
  { tagPairsResources :: [TagPair] -- ^ An array of objects containing resource_id and resource_type attributes
  } deriving (Show)

data TagPair = TagPair
  { tagPairResourceId   :: Id            -- ^ The identifier of a resource
  , tagPairResourceType :: ResourceType  -- ^ The type of the resource
  } deriving (Show)


instance FromJSON Tag where
  parseJSON (Object o) = Tag
                         <$> o .: "name"
                         <*> o .: "resources"

  parseJSON e          = failParse e

instance FromJSON TagResources where
  parseJSON (Object o) = TagResources
                         <$> o .:? "droplets" .!= TagDroplets 0 Nothing
                         <*> o .:? "volumes"  .!= TagVolumes  0 Nothing

  parseJSON e          = failParse e

instance FromJSON TagDroplets where
  parseJSON (Object o) = TagDroplets
                         <$> o .:  "count"
                         <*> o .:? "last_tagged"

  parseJSON e          = failParse e

instance FromJSON TagVolumes where
  parseJSON (Object o) = TagVolumes
                         <$> o .:  "count"
                         <*> o .:? "last_tagged"

  parseJSON e          = failParse e

instance FromJSON TagPairs where
  parseJSON (Object o) = TagPairs
                         <$> o .: "resources"

  parseJSON e         = failParse e

instance FromJSON TagPair where
  parseJSON (Object o) = TagPair
                         <$> o .: "resource_id"
                         <*> o .: "resource_type"

  parseJSON e          = failParse e

instance ToJSON Tag where
  toJSON Tag{..} = object [ "name"      .= tagName
                          , "resources" .= tagResources_
                          ]

instance ToJSON TagResources where
  toJSON TagResources{..} = object $ concat
    [ if isNothing (tagDropletsLastTagged tagDroplets) then [] else ["droplets" .= tagDroplets]
    , if isNothing (tagVolumesLastTagged tagVolumes)   then [] else ["volumes"  .= tagVolumes]
    ]

instance ToJSON TagDroplets where
  toJSON TagDroplets{..} = object [ "count"       .= tagDropletsCount
                                  , "last_tagged" .= tagDropletsLastTagged
                                  ]

instance ToJSON TagVolumes where
  toJSON TagVolumes{..} = object [ "count"       .= tagVolumesCount
                                 , "last_tagged" .= tagVolumesLastTagged
                                 ]

instance ToJSON TagPairs where
  toJSON TagPairs{..} = object [ "resources" .= tagPairsResources
                               ]

instance ToJSON TagPair where
  toJSON TagPair{..} = object [ "resource_id"   .= tagPairResourceId
                              , "resource_type" .= tagPairResourceType
                              ]


-- Utility
--
failParse :: (Show a1, Monad m) => a1 -> m a
failParse e = fail $ "cannot parse " <> show e
