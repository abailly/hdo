{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Network.DO.Commands where

import           Control.Comonad.Trans.Cofree
import           Control.Monad.Trans.Free
import           Network.DO.Pairing
import           Network.DO.Types
import           Prelude                      as P

-- functor for DO DSL
data DO a = ListKeys (Result [Key] -> a)
          | ListSizes (Result [Size] -> a)
          | ListImages (Result [Image] -> a)
          | ListRegions (Result [Region] -> a)
          deriving (Functor)

-- free transformer to embed effects
type DOT = FreeT DO

-- smart constructors
listKeys :: DO (Result [Key])
listKeys = ListKeys P.id

listSizes :: DO (Result [Size])
listSizes = ListSizes P.id

listImages  :: DO (Result [Image])
listImages = ListImages P.id

listRegions :: DO (Result [Region])
listRegions = ListRegions P.id

-- dual type, for creating interpreters
data CoDO m k = CoDO { listKeysH    :: (m (Result [Key]), k)
                     , listSizesH   :: (m (Result [Size]), k)
                     , listImagesH  :: (m (Result [Image]), k)
                     , listRegionsH :: (m (Result [Region]), k)
                     } deriving Functor

-- Cofree closure of CoDO functor
type CoDOT m = CofreeT (CoDO m)

-- pair DSL with interpreter within some monadic context
instance (Monad m) => PairingM (CoDO m) DO m where
  pairM f (CoDO ks _  _ _ )  (ListKeys k)   = pairM f ks k
  pairM f (CoDO _ szs _ _ )  (ListSizes k)  = pairM f szs k
  pairM f (CoDO _ _ imgs _ )  (ListImages k) = pairM f imgs k
  pairM f (CoDO _ _ _ rgns )  (ListRegions k) = pairM f rgns k
