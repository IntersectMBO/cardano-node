{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Cardano.Logging.Types where

import           Control.Tracer
import           Data.Map (Map)
import           Data.Text (Text)
import           Katip

type Trace m a = Tracer m (LoggingContext, a)

data LoggingContext = LoggingContext {
    lcContext  :: [Text]
  , lcSeverity :: Maybe Severity
  , lcPrivacy  :: Maybe PrivacyAnnotation
  , lcKatip    :: Maybe LogEnv
}

data LoggingContextKatip = LoggingContextKatip {
    lk       :: LoggingContext
  , lkLogEnv :: LogEnv
}

-- | Formerly known as verbosity
data DetailLevel =
      DBrief
    | DRegular
    | DDetailed
    deriving (Show, Eq, Ord, Bounded, Enum)

emptyLoggingContext :: LoggingContext
emptyLoggingContext = LoggingContext [] Nothing Nothing Nothing

data PrivacyAnnotation =
      Confidential -- confidential information - handle with care
    | Public       -- indifferent - can be public.
    deriving (Show, Eq, Ord, Bounded, Enum)

data SeverityF
    = DebugF                   -- ^ Debug messages
    | InfoF                    -- ^ Information
    | NoticeF                  -- ^ Normal runtime Conditions
    | WarningF                 -- ^ General Warnings
    | ErrorF                   -- ^ General Errors
    | CriticalF                -- ^ Severe situations
    | AlertF                   -- ^ Take immediate action
    | EmergencyF               -- ^ System is unusable
    | SilenceF
  deriving (Eq, Ord, Show, Enum, Bounded)

newtype Folding a b = Folding b
  deriving (ToObject)

-- Configuration options
data ConfigOption = ConfigOption {
    -- | Severity level for a message
    coSeverity    :: SeverityF
    -- | Detail level for a message
  , coDetailLevel :: DetailLevel
}


data TraceConfiguration = TraceConfiguration {
     -- | Options taken, if no more specific options are available
     tcDefaultOptions :: ConfigOption
     -- | Options specific to a certain namespace
  ,  tcOptions        :: Map Namespace ConfigOption

  --  Forwarder:
     -- Can their only be one forwarder? Use one of:

     --  Forward messages to the following address
--  ,  tcForwardTo :: RemoteAddr

     --  Forward messages to the following address
--  ,  tcForwardTo :: Map TracerName RemoteAddr

  --  ** Katip:

--  ,  tcDefaultScribe :: ScribeDefinition

--  ,  tcScripes :: Map TracerName -> ScribeDefinition

  --  EKG:
     --  Port for EKG server
--  ,  tcPortEKG :: Int

  -- Prometheus:
    --  Host/port to bind Prometheus server at
--  ,  tcBindAddrPrometheus :: Maybe (String,Int)
}
