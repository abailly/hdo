{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Network.DO.Droplets.Commands(DropletCommands,
                                    DropletCommandsT,CoDropletCommandsT,
                                    CoDropletCommands(..),
                                    listDroplets, createDroplet, destroyDroplet, dropletAction,
                                    showDroplet, getAction, listDropletSnapshots,
                                    dropletConsole) where

import Control.Comonad.Trans.Cofree
import Control.Monad.Trans.Free
import Network.DO.Pairing
import Network.DO.Types
import Prelude                       as P
import Network.HTTP.QueryString     (QueryString)

-- | Available commands for droplets
data DropletCommands a = ListDroplets QueryString (Result [Droplet] -> a)
                       | CreateDroplet BoxConfiguration (Result Droplet -> a)
                       | DestroyDroplet Id (Result () -> a)
                       | DropletAction Id Action (Result (ActionResult DropletActionType) -> a)
                       | GetAction Id Id (Result (ActionResult DropletActionType) -> a)
                       | ListSnapshots QueryString Id (Result [Image] -> a)
                       | Console Droplet (Result () -> a)
                       | ShowDroplet Id (Result Droplet -> a)
                       deriving (Functor)

-- free transformer to embed effects
type DropletCommandsT = FreeT DropletCommands

-- smart constructors
listDroplets :: QueryString -> DropletCommands (Result [Droplet])
listDroplets qs = ListDroplets qs P.id

createDroplet :: BoxConfiguration -> DropletCommands (Result Droplet)
createDroplet conf = CreateDroplet conf P.id

showDroplet :: Id -> DropletCommands (Result Droplet)
showDroplet did = ShowDroplet did P.id

destroyDroplet :: Id -> DropletCommands (Result ())
destroyDroplet did = DestroyDroplet did P.id

dropletAction :: Id -> Action -> DropletCommands (Result (ActionResult DropletActionType))
dropletAction did action = DropletAction did action P.id

dropletConsole :: Droplet -> DropletCommands (Result ())
dropletConsole droplet = Console droplet P.id

getAction :: Id -> Id -> DropletCommands (Result (ActionResult DropletActionType))
getAction did actId = GetAction did actId P.id

listDropletSnapshots :: QueryString -> Id -> DropletCommands (Result [Image])
listDropletSnapshots qs did = ListSnapshots qs did P.id


-- | Comonadic interpreter for @DropletCommands@
data CoDropletCommands m k = CoDropletCommands { listDropletsH   :: QueryString -> (m (Result [Droplet]), k)
                                               , createDropletH  :: BoxConfiguration -> (m (Result Droplet), k)
                                               , destroyDropletH :: Id -> (m (Result ()), k)
                                               , actionDropletH  :: Id -> Action -> (m (Result (ActionResult DropletActionType)), k)
                                               , getActionH      :: Id -> Id -> (m (Result (ActionResult DropletActionType)), k)
                                               , listSnapshotsH  :: QueryString -> Id -> (m (Result [Image]), k)
                                               , consoleH        :: Droplet -> (m (Result ()), k)
                                               , showDropletH    :: Id -> (m (Result Droplet), k)
                                               } deriving Functor

-- Cofree closure of CoDO functor
type CoDropletCommandsT m = CofreeT (CoDropletCommands m)

-- pair DSL with interpreter within some monadic context
instance (Monad m) => PairingM (CoDropletCommands m) DropletCommands m where
  pairM f (CoDropletCommands list _ _ _ _ _ _ _)       (ListDroplets qs k)       = pairM f (list qs) k
  pairM f (CoDropletCommands _ create _ _ _ _ _ _)     (CreateDroplet conf k)    = pairM f (create conf) k
  pairM f (CoDropletCommands _ _ destroy _ _ _ _ _)    (DestroyDroplet i k)      = pairM f (destroy i) k
  pairM f (CoDropletCommands _ _ _ action _ _ _ _)     (DropletAction i a k)     = pairM f (action i a) k
  pairM f (CoDropletCommands _ _ _ _ getA _ _ _)       (GetAction i i' k)        = pairM f (getA i i') k
  pairM f (CoDropletCommands _ _ _ _ _  snapshots _ _) (ListSnapshots qs i k)    = pairM f (snapshots qs i) k
  pairM f (CoDropletCommands _ _ _ _ _  _ console _)   (Console i k)             = pairM f (console i) k
  pairM f (CoDropletCommands _ _ _ _ _  _ _ showD)     (ShowDroplet i k)         = pairM f (showD i) k
