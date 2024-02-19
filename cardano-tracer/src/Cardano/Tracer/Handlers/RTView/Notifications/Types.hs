{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Cardano.Tracer.Handlers.RTView.Notifications.Types
  ( EmailSSL (..)
  , EmailSettings (..)
  , EventsSettings (..)
  , Event (..)
  , EventGroup (..)
  , EventsQueue
  , EventsQueues
  ) where

import           Cardano.Logging (SeverityS (..))
import           Cardano.Tracer.Handlers.RTView.Notifications.Timer
import           Cardano.Tracer.Types

import           Control.Concurrent.STM.TBQueue (TBQueue)
import           Control.Concurrent.STM.TVar (TVar)
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Map.Strict (Map)
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           GHC.Generics (Generic)

-- | Email settings for notifications.

data EmailSSL
  = TLS
  | STARTTLS
  | NoSSL
  deriving (Eq, Generic, FromJSON, ToJSON, Read, Show)

data EmailSettings = EmailSettings
  { esSMTPHost  :: !Text
  , esSMTPPort  :: !Int
  , esUsername  :: !Text
  , esPassword  :: !Text
  , esSSL       :: !EmailSSL
  , esEmailFrom :: !Text
  , esEmailTo   :: !Text
  , esSubject   :: !Text
  } deriving (Eq, Generic, FromJSON, ToJSON)

-- | Events settings for notifications.
--   They corresponds to UI switches in Events window.

data EventsSettings = EventsSettings
  { evsWarnings         :: !(Bool, PeriodInSec)
  , evsErrors           :: !(Bool, PeriodInSec)
  , evsCriticals        :: !(Bool, PeriodInSec)
  , evsAlerts           :: !(Bool, PeriodInSec)
  , evsEmergencies      :: !(Bool, PeriodInSec)
  , evsNodeDisconnected :: !(Bool, PeriodInSec)
  } deriving (Generic, FromJSON, ToJSON)

-- | Event we should notify about.
data Event = Event
  { evNodeId   :: !NodeId
  , evTime     :: !UTCTime
  , evSeverity :: !SeverityS
  , evMessage  :: !Text
  } deriving (Eq, Show)

-- | The queue for events we should notify about.
type EventsQueue = TBQueue Event

data EventGroup
  -- Common problems
  = EventWarnings
  | EventErrors
  | EventCriticals
  | EventAlerts
  | EventEmergencies
  -- Disconnect
  | EventNodeDisconnected
  deriving (Eq, Ord)

-- | ...
type EventsQueues = TVar (Map EventGroup (EventsQueue, Timer))
