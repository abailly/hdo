{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Network.DO.Commands where

import           Control.Comonad.Trans.Cofree
import           Control.Monad.Trans.Free
import           Network.DO.Pairing
import           Network.DO.Types
import           Network.HTTP.QueryString     (QueryString)
import           Prelude                      as P

-- functor for DO DSL
data DO a = ListKeys QueryString (Result [Key] -> a)
          | ListSizes QueryString (Result [Size] -> a)
          | ListImages QueryString (Result [Image] -> a)
          | ListRegions QueryString (Result [Region] -> a)
          deriving (Functor)

-- free transformer to embed effects
type DOT = FreeT DO

-- smart constructors
listKeys :: QueryString -> DO (Result [Key])
listKeys q = ListKeys q P.id

listSizes :: QueryString -> DO (Result [Size])
listSizes q = ListSizes q P.id

listImages :: QueryString -> DO (Result [Image])
listImages q = ListImages q P.id

listRegions :: QueryString -> DO (Result [Region])
listRegions q = ListRegions q P.id

-- dual type, for creating interpreters
data CoDO m k = CoDO { listKeysH    :: QueryString -> (m (Result [Key]), k)
                     , listSizesH   :: QueryString -> (m (Result [Size]), k)
                     , listImagesH  :: QueryString -> (m (Result [Image]), k)
                     , listRegionsH :: QueryString -> (m (Result [Region]), k)
                     } deriving Functor

-- Cofree closure of CoDO functor
type CoDOT m = CofreeT (CoDO m)

-- pair DSL with interpreter within some monadic context
instance (Monad m) => PairingM (CoDO m) DO m where
  pairM f (CoDO ks _  _ _ )  (ListKeys q k)     = pairM f (ks q) k
  pairM f (CoDO _ szs _ _ )  (ListSizes q k)    = pairM f (szs q) k
  pairM f (CoDO _ _ imgs _ )  (ListImages q k)  = pairM f (imgs q) k
  pairM f (CoDO _ _ _ rgns )  (ListRegions q k) = pairM f (rgns q) k
