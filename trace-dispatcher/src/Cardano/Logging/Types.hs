{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Cardano.Logging.Types where

import           Control.Tracer
import qualified Data.Map as Map
import           Data.Text (Text)
import           Katip (LogEnv, Severity, ToObject)

type Context = [Text]

-- | Configurable tracer which carries a context while tracing
type Trace m a = Tracer m (LoggingContext, Either TraceControl a)

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
data ConfigOption =
    -- | Severity level (default is WarningF)
    CoSeverity SeverityF
    -- | Detail level (Default is DRegular)
  | CoDetailLevel DetailLevel
    -- | Privacy level (Default is Public)
  | CoPrivacy Privacy
    -- | Defined in messages per second (Defaul is 10)
  | CoMaxFrequency Int

data TraceConfig = TraceConfig {
     -- | Options specific to a certain namespace
     tcOptions        :: Map.Map Context [ConfigOption]

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

emptyTraceConfig = TraceConfig {tcOptions = Map.empty}

-- | When configuring a net of tracers, it should be run with Config on all
-- entry points first, and then with Optimize. When reconfiguring it needs to
-- run Reset followed by Config followed by Optimize
data TraceControl =
    Reset
  | Config TraceConfig
  | Optimize
