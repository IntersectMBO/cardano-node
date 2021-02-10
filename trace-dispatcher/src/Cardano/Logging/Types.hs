{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Cardano.Logging.Types where

import           Control.Tracer
import           Data.Map (Map)
import           Data.Text (Text)
import           Katip (LogEnv, Severity, ToObject)

type Context = [Text]

-- | Configurable tracer which carries a context while tracing
type Trace m a = Tracer m (LoggingContext, Either TraceConfig a)

data LoggingContext = LoggingContext {
    lcContext  :: Context
  , lcSeverity :: Maybe Severity
  , lcPrivacy  :: Maybe Privacy
}

emptyLoggingContext :: LoggingContext
emptyLoggingContext = LoggingContext [] Nothing Nothing

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

data Privacy =
      Confidential -- confidential information - handle with care
    | Public       -- can be public.
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

-- Configuration options for individual
data ConfigOption = ConfigOption {
    -- | Severity level
    coSeverity     :: SeverityF
    -- | Detail level
  , coDetailLevel  :: DetailLevel
    -- | Privacy level
  , coPrivacy      :: Privacy
    -- | Defined in messages per second
  , coMaxFrequency :: Int
}

data TraceConfig = TraceConfig {
     -- | Options taken, if no more specific options are available
     tcDefaultOptions :: ConfigOption
     -- | Options specific to a certain namespace
  ,  tcOptions        :: Map Context ConfigOption

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
